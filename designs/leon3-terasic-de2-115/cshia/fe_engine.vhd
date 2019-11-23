LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
LIBRARY CSHIA;
USE CSHIA.ptag_gen_pkg.ALL;
USE CSHIA.handler_pkg.ALL;
USE CSHIA.bch.ALL;
USE CSHIA.ecc.ALL;

ENTITY fe_engine IS
	PORT (clk,
		rstn,
		reboot					: IN STD_LOGIC;
		helper_data_dec			: IN STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
		extracted_rword			: IN STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0); 
		extracted_key			: IN STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
		recovered_key			: OUT STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
		helper_data_enc			: OUT STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
		done					: OUT STD_LOGIC
	);
END ENTITY;

ARCHITECTURE rtl OF fe_engine IS
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
	signal	increment_addr	: std_logic_vector(10 downto 0);
	signal	debug_data		: std_logic_vector(63 downto 0);
	signal	log				: std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------

	TYPE states IS (S_START, S_DEC, S_ENC, S_FINALIZE);
	
	SIGNAL enc_enable, dec_enable, enc_ready, dec_ready : STD_LOGIC;
	
	SIGNAL rword, tmp_rword, aux_syndrome : STD_LOGIC_VECTOR (RWORD_LENGTH-1 DOWNTO 0);
	SIGNAL syndrome, tmp_syndrome : STD_LOGIC_VECTOR (RWORD_LENGTH-2 DOWNTO 0);
	-- Recovered codeword from the helper data.
	SIGNAL codeword : STD_LOGIC_VECTOR (C_KEY_LENGTH-1 DOWNTO 0);
	
	SIGNAL rword_parity, syndrome_parity : STD_LOGIC;
	
	SIGNAL state : states;
	signal global_dec_rst_n : std_logic;
	signal global_enc_rst_n : std_logic;

BEGIN
	global_dec_rst_n <= NOT (rstn AND dec_enable);
	global_enc_rst_n <= NOT (rstn AND enc_enable);

	BCHDecode_inst:
	BCHDecoder PORT MAP (
		clk			=> clk,
		reset		=> global_dec_rst_n,
		codeword	=> codeword (C_KEY_LENGTH-1 DOWNTO 1),
		data		=> tmp_rword,
		ready		=> dec_ready
	);
	
	BCHEncode_inst:
	BCHEncoder PORT MAP (
		clk			=> clk,
		reset		=> global_enc_rst_n,
		data		=> rword,
		syndrome	=> tmp_syndrome,
		ready		=> enc_ready
	);
	
	DecodeEncodeSequency:
	PROCESS (clk, rstn)
	BEGIN
		IF (rstn = '0') THEN
			dec_enable		<= '0';
			enc_enable		<= '0';
			done			<= '0';
			state			<= S_START;
			rword			<= (OTHERS => '0');
			syndrome		<= (OTHERS => '0');
		ELSIF (RISING_EDGE(clk)) THEN 
			CASE state IS
			WHEN S_START =>
				IF reboot = '1' THEN
					rword	<= (OTHERS => '0');
					state	<= S_DEC;
--COMMENTED--				debug_data			<= helper_data_dec;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				ELSE
					rword	<= extracted_key;
					state	<= S_ENC;
--COMMENTED--				debug_data			<= extracted_key;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				END IF;
				syndrome	<= (OTHERS => '0');
				done		<= '0';
			WHEN S_DEC =>
				dec_enable <= '1';
				enc_enable <= '0';
				IF dec_ready = '1' THEN
					state		<= S_FINALIZE;
					rword		<= tmp_rword;
					
--COMMENTED--				debug_data			<= tmp_rword;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				END IF;
			WHEN S_ENC =>
				dec_enable <= '0';
				enc_enable <= '1';
				IF enc_ready = '1' THEN
					state		<= S_FINALIZE;
					syndrome	<= tmp_syndrome;

--COMMENTED--				debug_data			<= tmp_syndrome & '0';	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				END IF;
			WHEN OTHERS =>
--COMMENTED--				debug_data			<= extracted_rword;	
--COMMENTED--				increment_addr		<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_ADDR_DBUG));
				dec_enable <= '0';
				enc_enable <= '0';
				done <= '1';
--				state <= S_START;
			END CASE;
		END IF;
	END PROCESS;

	SyndromeParitiy:
	PROCESS (clk, rstn, state)
		VARIABLE par : STD_LOGIC := '0';
	BEGIN
		IF (rstn = '0') THEN
			syndrome_parity <= '0';
		ELSIF (RISING_EDGE(clk)) THEN 
			IF state = S_FINALIZE THEN
				FOR i IN 0 TO RWORD_LENGTH-2 LOOP
					par := syndrome (i) XOR par;
				END LOOP;	
			END IF;
			syndrome_parity <= par;
		END IF;
	END PROCESS;

	recovered_key	<= rword;
	helper_data_enc	<= (syndrome & syndrome_parity) xor extracted_rword;
 
	aux_syndrome	<= helper_data_dec xor extracted_rword;
	codeword		<= aux_syndrome(RWORD_LENGTH-1 downto 1) & extracted_key & aux_syndrome(0);

--COMMENTED--debug_memory_inst : debug_memory PORT MAP (
--COMMENTED--		address	 => increment_addr,
--COMMENTED--		clock	 => clk,
--COMMENTED--		data	 => debug_data,
--COMMENTED--		wren	 => '1',
--COMMENTED--		q		 => open
--COMMENTED--	);
	
--	RWordParitiy:
--	PROCESS (clk, rstn, state)
--		VARIABLE par : STD_LOGIC := '0';
--	BEGIN
--		IF (rstn = '0') THEN
--			rword_parity <= '0';
--		ELSIF (RISING_EDGE(clk)) THEN 
--			IF state = S_FINALIZE THEN
--				FOR i IN 0 TO RWORD_LENGTH-1 LOOP
--					par := rword (i) XOR par;
--				END LOOP;	
--			END IF;
--			rword_parity <= par;
--		END IF;
--	END PROCESS;
	
	-- Recovering the codeword from the helper data
--	codeword <= helper_data XOR extracted_key;
	
	-- Assembling the corrected codeword with the parity bit.
--	codeword_p <= (syndrome & rword & (rword_parity XOR syndrome_parity));
END ARCHITECTURE;
	
