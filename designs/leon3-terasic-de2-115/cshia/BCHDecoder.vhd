LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
LIBRARY CSHIA;
USE CSHIA.bch.ALL;

ENTITY BCHDecoder IS
		PORT (clk, reset : IN STD_LOGIC;
			codeword : IN STD_LOGIC_VECTOR (RWORD_LENGTH*2-2 DOWNTO 0);
			data : OUT STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
			ready : OUT STD_LOGIC
		);
END ENTITY;

ARCHITECTURE Decode OF BCHDecoder IS
	SIGNAL input_bit, vdout, output_bit : BIT;
	SIGNAL count : INTEGER RANGE 0 TO RWORD_LENGTH;
	SIGNAL c_global : INTEGER RANGE 0 TO RWORD_LENGTH*3-1;
BEGIN
	serialization:
	PROCESS (clk, reset)
	BEGIN
		IF reset = '1' THEN
			c_global <= 0;
		ELSIF (RISING_EDGE(clk)) THEN
			IF vdout = '0' THEN
-- 				c_global <= c_global + 1;

			if c_global =  RWORD_LENGTH*3-1 then
			  c_global <= 0;
			else
			  c_global <= c_global + 1;
			end if;
			
			END IF;
		END IF;
	END PROCESS;
	
	bch_decoding:
	dec PORT MAP (
		clk		=> TO_BIT(clk),
		reset		=> TO_BIT(reset),
		din		=> input_bit,
		vdout		=> vdout,
		dout		=> output_bit
	);
	
	input_bit <= TO_BIT(codeword(c_global)) WHEN c_global < RWORD_LENGTH*2-1 ELSE '0';
	
	buffering_output:
	PROCESS (clk, reset, output_bit)
	BEGIN
		IF reset = '1' THEN
			data <= (OTHERS => '0');
			count <= 0;
			ready <= '0';
		ELSIF (RISING_EDGE(clk)) THEN
			IF (vdout = '1') AND (count < RWORD_LENGTH) THEN
				data(count) <= TO_STDULOGIC(output_bit);
				count <= count + 1;
			ELSE
				count <= 0;
			END IF;
				
			IF (count = RWORD_LENGTH-1) THEN
				ready <= '1';
			END IF;
		END IF;
	END PROCESS;
END ARCHITECTURE;
