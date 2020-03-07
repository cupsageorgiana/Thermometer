library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Controller is
	Generic (CLOCKFREQ : natural := 100); 
	Port (
		tmp_scl : inout STD_LOGIC;
		tmp_sda : inout STD_LOGIC;
        seg : out std_logic_vector(6 downto 0);
	    an : out std_logic_vector(7 downto 0);
	    led : out std_logic_vector(3 downto 0);
		clk_i : in STD_LOGIC;
		SRST_I : in STD_LOGIC
	);
end Controller;

architecture Behavioral of Controller is

component TempSensorCtl is
	Generic (CLOCKFREQ : natural := 100);
	Port (
		TMP_SCL : inout STD_LOGIC;
		TMP_SDA : inout STD_LOGIC;
		CLK_I : in STD_LOGIC;
		SRST_I : in STD_LOGIC;
        TEMP_O : out STD_LOGIC_VECTOR(12 downto 0); --12-bit two's complement temperature with sign bit
		RDY_O : out STD_LOGIC;	--'1' when there is a valid temperature reading on TEMP_O
		ERR_O : out STD_LOGIC); --'1' if communication error
end component;

--component MonitorFPGA is
--    Port ( binIN : in  STD_LOGIC_VECTOR (11 downto 0);
--           ones : out  STD_LOGIC_VECTOR (3 downto 0);
--           tens : out  STD_LOGIC_VECTOR (3 downto 0);
--           hundreds : out  STD_LOGIC_VECTOR (3 downto 0);
--           thousands : out  STD_LOGIC_VECTOR (3 downto 0)
--          );
--end component;

component Display is
port(
digit0: in std_logic_vector(3 downto 0);
digit1: in std_logic_vector(3 downto 0);
digit2: in std_logic_vector(3 downto 0);
digit3: in std_logic_vector(3 downto 0);
digit4: in std_logic_vector(3 downto 0);
digit5: in std_logic_vector(3 downto 0);
digit6: in std_logic_vector(3 downto 0);
digit7: in std_logic_vector(3 downto 0);
CLK : in std_logic;
seg: out std_logic_vector(0 to 6);
an : out std_logic_vector(7 downto 0)
);
end component;

signal temperatura,aux,print: std_logic_vector(15 downto 0);
signal c1:std_logic_vector(7 downto 0);
signal c2:std_logic_vector(3 downto 0):="0000";
signal c3:std_logic_vector(3 downto 0):="1100";
signal c4:std_logic_vector(3 downto 0):="1010";
begin

FSM: TempSensorCtl
	Generic map (CLOCKFREQ => 100) -- input CLK frequency in MHz
	Port map(
		TMP_SCL => TMP_SCL ,
		TMP_SDA => TMP_SDA ,
        TEMP_O => aux(12 downto 0),  --12-bit two's complement temperature with sign bit
		RDY_O => led(0),	--'1' when there is a valid temperature reading on TEMP_O
		ERR_O => led(1) , --'1' if communication error
		CLK_I => CLK_I,
		SRST_I => SRST_I);

conversie_grade: process(temperatura)
variable x:integer;
variable w:integer;
variable y:integer;
variable z:integer;
begin
x := to_integer(unsigned(aux));
w := x*100;
y := w/16;
z := y-700;
temperatura<=std_logic_vector(to_unsigned(z,temperatura'length));
end process;
		
--Monitor : MonitorFPGA port map(binIN=>temperatura(11 downto 0),ones=>print(15 downto 12),tens=>print(11 downto 8),hundreds=>print(7 downto 4),thousands=>print(3 downto 0));

c1<=aux(7 downto 0) +"00000110";

Afisare : Display Port map(
    digit0=>c3,
    digit1=>c4,
    digit2=>c1(3 downto 0),
    digit3=>c1(7 downto 4),
    digit4=>c2,
    digit5=>c2,
    digit6=>c2,
    digit7=>c2,
    Clk=>CLK_I,
    seg=>seg,
    an=>an);
end Behavioral;   