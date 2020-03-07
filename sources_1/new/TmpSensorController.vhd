library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.math_real.all;
use IEEE.std_logic_arith.all;

use work.TWIUtils.ALL;


-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity TempSensorCtl is
	Generic (CLOCKFREQ : natural := 100); -- CLK, frecventa in MHz
	Port (
		TMP_SCL : inout STD_LOGIC;
		TMP_SDA : inout STD_LOGIC;
		
        CLK_I : in STD_LOGIC;
		SRST_I : in STD_LOGIC;

		TEMP_O : out STD_LOGIC_VECTOR(12 downto 0); -- temp pe 12 biti in C2 cu semn
		RDY_O : out STD_LOGIC;
		ERR_O : out STD_LOGIC); --'1' daca eroare
		
end TempSensorCtl;

architecture Behavioral of TempSensorCtl is

-- TWI Controller 
component TWI
   generic 
   (
		CLOCKFREQ : natural := 50;  -- input CLK in MHz
		ATTEMPT_SLAVE_UNBLOCK : boolean := false 	
	);
	port (
		MSG_I : in STD_LOGIC; 
		STB_I : in STD_LOGIC; -- strobe
		A_I : in  STD_LOGIC_VECTOR (7 downto 0); --address input bus
		D_I : in  STD_LOGIC_VECTOR (7 downto 0); --data input bus
		D_O : out  STD_LOGIC_VECTOR (7 downto 0); --data output bus
		DONE_O : out  STD_LOGIC; --done status signal
        ERR_O : out  STD_LOGIC; --error status
		CLK : in std_logic;
		SRST : in std_logic;

-- TWI bus signals
		SDA : inout std_logic; --TWI SDA
		SCL : inout std_logic --TWI SCL
	);
end component;

-- Definitions for the I2C initialization vector

	constant IRD : std_logic := '1'; -- init read
	constant IWR : std_logic := '0'; -- init write
	
	constant ADT7420_ADDR : std_logic_vector(7 downto 1)     := "1001011"; -- TWI Slave Address
	constant ADT7420_RID : std_logic_vector(7 downto 0)      := x"0B"; -- ID Register Address for the ADT7420
	constant ADT7420_RRESET : std_logic_vector(7 downto 0)   := x"2F"; -- Software Reset Register
	constant ADT7420_RTEMP : std_logic_vector(7 downto 0)    := x"00"; -- Temperature Read MSB Address
	constant ADT7420_ID : std_logic_vector(7 downto 0)       := x"CB"; -- ADT7420 Manufacturer ID
	
	constant DELAY : NATURAL := 1; --ms
	constant DELAY_CYCLES : NATURAL := natural(ceil(real(DELAY*1000*CLOCKFREQ)));
	constant RETRY_COUNT : NATURAL := 10;

   -- State Machine states definition
   type state_type is (
                        stIdle, -- Idle State
                        stInitReg,  -- Send register address from the init vector
                        stInitData, -- Send data byte from the init vector
                        stRetry,    -- Retry state reached when there is a bus error, will retry RETRY_COUNT times
                        stReadTempR,  -- Send temperature register address
                        stReadTempD1, -- Read temperature MSB
                        stReadTempD2, -- Read temperature LSB
                        stError -- Error state when reached when there is a bus error after a successful init; stays here until reset
                        ); 
   signal state, nstate : state_type; 

	
   constant NO_OF_INIT_VECTORS : natural := 3; -- number of init vectors in TempSensInitMap
   constant DATA_WIDTH : integer := 1 + 8 + 8; -- RD/WR bit + 1 byte register address + 1 byte data
   constant ADDR_WIDTH : natural := natural(ceil(log(real(NO_OF_INIT_VECTORS), 2.0)));
	
	type TempSensInitMap_type is array (0 to NO_OF_INIT_VECTORS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
	signal TempSensInitMap: TempSensInitMap_type := (
		IRD & x"0B" & x"CB", -- Read ID R[0x0B]=0xCB
		IWR & x"2F" & x"00", -- Reset R[0x2F]=don't care
		IRD & x"0B" & x"CB" -- Read ID R[0x0B]=0xCB
		);	
	
	signal initWord: std_logic_vector (DATA_WIDTH-1 downto 0);
	signal initA : natural range 0 to NO_OF_INIT_VECTORS := 0; --init vector index
	signal initEn : std_logic;
	
	-- Two-Wire Controller -> semnale
	signal twiMsg, twiStb, twiDone, twiErr : std_logic;
	signal twiDi, twiDo, twiAddr : std_logic_vector(7 downto 0);

	-- Asteapta contorul utilizat intre incercarile de reincercare
	signal waitCnt : natural range 0 to DELAY_CYCLES := DELAY_CYCLES;
	signal waitCntEn : std_logic;
   
	signal retryCnt : natural range 0 to RETRY_COUNT := RETRY_COUNT;
	signal retryCntEn : std_logic;
	
	-- Registru temporar pentru stocarea datelor primite
	signal tempReg : std_logic_vector(15 downto 0) := (others => '0');
	
	-- Flag care indica faptul ca sunt disponibile date noi privind temperatura
   signal fReady : boolean := false;
   -- temp max 
   constant TEMP_MAX	: std_logic_vector (23 downto 0) := X"000500"; -- 80C * 16

   -- Valoarea temperaturii convertita si scalata
   signal temp_value 		: std_logic_vector(11 downto 0);

   -- Sincronizare temperatura de intrare cu ceasul
   signal temp_sync0, temp_sync : std_logic_vector(12 downto 0);

   -- Semnalul de stocare a temperaturii senzorului de temp sau a accelerometrului limitat intre 0 ° C ?i 80 ° C * 16
   signal temp_capped	: std_logic_vector(11 downto 0);

   -- temp pt a converti
  signal temp_out: std_logic_vector(12 downto 0);
  
begin

-- Iesiri

temp_out <= tempReg(15 downto 3);

RDY_O <= '1' when fReady else
			'0';
ERR_O <= '1' when state = stError else
			'0';


-- I2C Master Controller

Inst_TWICtl : TWI
	generic map (
		ATTEMPT_SLAVE_UNBLOCK => true,
		CLOCKFREQ => 100
	)
	port map (
		MSG_I => twiMsg,
		STB_I => twiStb,
		A_I => twiAddr,
		D_I => twiDi,
		D_O => twiDo,
		DONE_O => twiDone,
      ERR_O => twiErr,
		--ERRTYPE_O => open,
		CLK => CLK_I,
		SRST => SRST_I,
		SDA => TMP_SDA,
		SCL => TMP_SCL
	);


-- Initializare Map RAM

	initWord <= TempSensInitMap(initA);

	InitA_CNT: process (CLK_I) 
	begin
		if Rising_Edge(CLK_I) then
			if (state = stIdle or initA = NO_OF_INIT_VECTORS) then
				initA <= 0;
			elsif (initEn = '1') then
				initA <= initA + 1;
			end if;
		end if;
	end process;
	

-- Delay and Retry Counters
	
	Wait_CNT: process (CLK_I) 
	begin
		if Rising_Edge(CLK_I) then
			if (waitCntEn = '0') then
				waitCnt <= DELAY_CYCLES;
			else
				waitCnt <= waitCnt - 1;
			end if;
		end if;
	end process;
	
	Retry_CNT: process (CLK_I) 
	begin
		if Rising_Edge(CLK_I) then
			if (state = stIdle) then
				retryCnt <= RETRY_COUNT;
			elsif (retryCntEn = '1') then
				retryCnt <= retryCnt - 1;
			end if;
		end if;
	end process;


-- Temperature Registers

	TemperatureReg: process (CLK_I) 
	variable temp : std_logic_vector(7 downto 0);
	begin
		if Rising_Edge(CLK_I) then
			--MSB
			if (state = stReadTempD1 and twiDone = '1' and twiErr = '0') then
				temp := twiDo;
			end if;
			--LSB
			if (state = stReadTempD2 and twiDone = '1' and twiErr = '0') then
				tempReg <= temp & twiDo;
			end if;
		end if;
	end process;
	

-- Ready Flag
	
	ReadyFlag: process (CLK_I) 
	begin
		if Rising_Edge(CLK_I) then
			if (state = stIdle or state = stError) then
				fReady <= false;
			elsif (state = stReadTempD2 and twiDone = '1' and twiErr = '0') then
				fReady <= true;
			end if;
		end if;
	end process;	
	

-- Initializare FSM si temp continua citita

   SYNC_PROC: process (CLK_I)
   begin
      if (CLK_I'event and CLK_I = '1') then
         if (SRST_I = '1') then --reset activ pe 1
            state <= stIdle;
         else
            state <= nstate;
         end if;        
      end if;
   end process;
 
   OUTPUT_DECODE: process (state, initWord, twiDone, twiErr, twiDo, retryCnt, waitCnt, initA)
   begin
		twiStb <= '0'; --byte send/receive strobe
		twiMsg <= '0'; --new transfer request (repeated start)
		waitCntEn <= '0'; --wait countdown enable
		twiDi <= "--------"; --byte to send
		twiAddr <= ADT7420_ADDR & '0'; --I2C device address with R/W bit
		initEn <= '0'; --increase init map address
		retryCntEn <= '0'; --retry countdown enable
		
		case (state) is
         when stIdle =>
			
         when stInitReg => -- aceasta stare trimite adresa de inregistrare din vectorul init curent
            twiStb <= '1';
				twiMsg <= '1';
				twiAddr(0) <= IWR; -- Register address is a write
				twiDi <= initWord(15 downto 8);
			when stInitData => --aceasta stare trimite octetul de date din vectorul init curent
            twiStb <= '1';
				twiAddr(0) <= initWord(initWord'high); --poate fi scris sau citit
				twiDi <= initWord(7 downto 0);
				if (twiDone = '1' and
					(twiErr = '0' or (initWord(16) = IWR and initWord(15 downto 8) = ADT7420_RRESET)) and
					(initWord(initWord'high) = IWR or twiDo = initWord(7 downto 0))) then
					initEn <= '1';
				end if;
			when stRetry=> -- in cazul unei erori I2C in timpul initializarii, aceasta stare va fi atinsa
				if (retryCnt /= 0) then				
					waitCntEn <= '1';
					if (waitCnt = 0) then
						retryCntEn <= '1';
					end if;
				end if;
			
			when stReadTempR => -- aceasta stare trimite adresa de inregistrare a temperaturii
				twiStb <= '1';
				twiMsg <= '1';
				twiDi <= ADT7420_RTEMP;
				twiAddr(0) <= IWR; -- Register address is a write				
			when stReadTempD1 => -- aceasta stare citeste MSB temp 
				twiStb <= '1';
				twiAddr(0) <= IRD;
			when stReadTempD2 => -- aceasta stare citeste LSB temp 
            twiStb <= '1';
				twiAddr(0) <= IRD;
				
			when stError => -- in cazul unei erori I2C in timpul sondajului de temp
				null; -- ramai aici
		end case;
			
   end process;
 
   NEXT_STATE_DECODE: process (state, twiDone, twiErr, initWord, twiDo, retryCnt, waitCnt)
   begin
      -- declara starea implicita pentru nstate pentru a evita blocarea
      nstate <= state;  --default e pt a sta in starea curenta

      case (state) is
         when stIdle =>
            nstate <= stInitReg;
				
         when stInitReg =>
            if (twiDone = '1') then
					if (twiErr = '1') then
						nstate <= stRetry;
					else
						nstate <= stInitData;
					end if;
				end if;
	
			when stInitData =>
            if (twiDone = '1') then
					if (twiErr = '1') then
						nstate <= stRetry;
					else
						if (initWord(initWord'high) = IRD and twiDo /= initWord(7 downto 0)) then
							nstate <= stRetry;
						elsif (initA = NO_OF_INIT_VECTORS-1) then
							nstate <= stReadTempR;
						else
							nstate <= stInitReg;
						end if;
					end if;
				end if;				
			when stRetry =>
				if (retryCnt = 0) then
					nstate <= stError;
				elsif (waitCnt = 0) then
					nstate <= stInitReg; --new retry attempt
				end if;
				
			when stReadTempR =>
            if (twiDone = '1') then
					if (twiErr = '1') then
						nstate <= stError;
					else
						nstate <= stReadTempD1;
					end if;
				end if;
			when stReadTempD1 =>
            if (twiDone = '1') then
					if (twiErr = '1') then
						nstate <= stError;
					else
						nstate <= stReadTempD2;
					end if;
				end if;
			when stReadTempD2 =>
            if (twiDone = '1') then
					if (twiErr = '1') then
						nstate <= stError;
					else
						nstate <= stReadTempR;
					end if;
				end if;

         when stError =>
				null; --stai
				
         when others =>
            nstate <= stIdle;
      end case;      
   end process;
   
   
   TEMP_ACC:  
   process(CLK_I)
begin
	if CLK_I'EVENT and CLK_I = '1' then
			temp_sync0 <= temp_out; --synchronize with pxl_clk domain
			temp_sync <= temp_sync0;
			
			if (temp_sync(temp_sync'high) = '1') then --if negative, cap to 0
				temp_capped <= (others => '0');
			elsif (temp_sync(temp_sync'high-1 downto 0) > TEMP_MAX) then -- if too big, cap to maximum scale /0.0625
				temp_capped <= TEMP_MAX(temp_capped'range);
			else
				temp_capped <= temp_sync(temp_sync'high-1 downto 0); --get rid of the sign bit
			end if;
			
	end if;
end process;

--	temp_value <= temp_capped; -- * 0.0625 (1/2^4); 

    TEMP_O<= "0000" & temp_out(12 downto 4);


end Behavioral;
