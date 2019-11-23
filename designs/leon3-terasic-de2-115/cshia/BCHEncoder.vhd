LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
LIBRARY CSHIA;
USE CSHIA.bch.ALL;

ENTITY BCHEncoder IS
		PORT (clk, reset : IN STD_LOGIC;
			data : IN STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
			syndrome: OUT STD_LOGIC_VECTOR (RWORD_LENGTH-2 DOWNTO 0);
			ready : OUT STD_LOGIC
		);
END ENTITY;

ARCHITECTURE Encode OF BCHEncoder IS
	SIGNAL input_bit, vdin, output_bit : BIT;
	SIGNAL count : INTEGER RANGE 0 TO RWORD_LENGTH-1;
	SIGNAL c_global : INTEGER RANGE 0 TO RWORD_LENGTH*2;
BEGIN
	serialization:
	PROCESS (clk, reset)
	BEGIN
		IF reset = '1' THEN
			count <= 0;
			c_global <= 0;
		ELSIF (RISING_EDGE(clk)) THEN
			
			if c_global =  RWORD_LENGTH*2 then
			  c_global <= 0;
			else
			  c_global <= c_global + 1;
			end if;
			
			IF count < RWORD_LENGTH-1 THEN 
				count <= count + 1;
			ELSE
				count <= 0;
			END IF;
		END IF;
	END PROCESS;
	
	bch_encoding:
	enc PORT MAP (clk => TO_BIT(clk), reset => TO_BIT(reset), din => input_bit, vdin => vdin, dout => output_bit);
	
	input_bit <= TO_BIT(data(count));
	
	buffering_output:
	PROCESS (clk, reset, output_bit)
	BEGIN
		IF reset = '1' THEN
			syndrome <= (OTHERS => '0');
		ELSIF (RISING_EDGE(clk)) THEN
-- 			syndrome(count-1) <= TO_STDULOGIC(output_bit);
		  if count > 0 then 
			syndrome(count-1) <= TO_STDULOGIC(output_bit);
		  end if;
		END IF;
	END PROCESS;

	ready <= '1' WHEN c_global = RWORD_LENGTH*2 ELSE '0';
END ARCHITECTURE;
