library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;


entity Display is
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
end Display;


architecture Behavioral of Display is

signal cnt : std_logic_vector(15 downto 0);
signal aux : std_logic_vector(3 downto 0);

begin
		
	process(CLK)
		begin
			if rising_edge( CLK ) then
				cnt <= cnt + 1;
			end if;
		end 
	process;
	
	process(cnt( 15 ) , cnt( 14 ))
		begin
			case cnt( 15 downto 13 ) is
			
			when  "000"  => an <= "11111110";
			when  "001"  => an <= "11111101";
			when  "010"  => an <= "11111011";
			when  "011"  => an <= "11110111";
			when  "100"  => an <= "11101111";
			when  "101"  => an <= "11011111";
			when  "110"  => an <= "10111111";
			when  "111"  => an <= "01111111";
			
			when others => an <= "00000000";
			
			end case;
		end 
	process;
	
	process(cnt( 15 ) , cnt( 14 ))
		begin
			case cnt( 15 downto 13 ) is
			
			when  "000"  => aux <= digit0;
			when  "001"  => aux <= digit1;
			when  "010"  => aux <= digit2;
			when  "011"  => aux <= digit3;
			when  "100"  => aux <= digit4;
			when  "101"  => aux <= digit5;
			when  "110"  => aux <= digit6;
			when  "111"  => aux <= digit7;
			
			when others => aux <= "1111";
			
			end case;
		end 
	process;
	
	process (aux)
		begin
			case  aux is
				when "0001" => seg <="1111001";   --1
				when "0010" => seg <="0100100";   --2
				when "0011" => seg <="0110000";   --3
				when "0100" => seg <="0011001";   --4
				when "0101" => seg <="0010010";   --5
				when "0110" => seg <="0000010";   --6
				when "0111" => seg <="1111000";   --7
				when "1000" => seg <="0000000";   --8
				when "1001" => seg <="0010000";   --9
				when "1010" => seg <="0011100";   --A
				when "1011" => seg <="0000011";   --b
				when "1100" => seg <="1000110";   --C
				when "1101" => seg <="0100001";   --d
				when "1110" => seg <="0000110";   --E
				when "1111" => seg <="0001110";   --F
				when others => seg <="1000000";   --0
			end case;
		end 
	process;
	
end Behavioral;
		
				