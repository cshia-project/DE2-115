library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library CSHIA;
use CSHIA.SipHash_2_4.all;
use CSHIA.handler_pkg.all;
use CSHIA.DBUG_PKG.all;

entity ptag_generator is
	port (clk,
		rstn					: in std_logic;
		address					: in std_logic_vector (C_ADDR_WIDTH-1 downto 0);
		key						: in std_logic_vector (C_KEY_LENGTH-1 downto 0);
		memory_block			: in std_logic_vector (C_LINE_SIZE*C_WORD_WIDTH-1 downto 0);
		enable_computation		: in std_logic;
--		load_key_done,
		ptag_ready				: out std_logic;
		input_data				: out std_logic_vector (C_SIPHASH_BLOCK_WIDTH-1 downto 0);
		ptag					: out std_logic_vector (C_PTAG_WIDTH-1 downto 0)
	);
end entity;

architecture PTAGGEN of ptag_generator is

-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------
	signal	increment_addr	: std_logic_vector(C_S_ADDR_DBUG-1 downto 0);
	signal	debug_data		: std_logic_vector(63 downto 0);
	signal	log				: std_logic_vector(15 downto 0);
-------------------------------------------------------------------------------
------------------------------------ DEBUG ------------------------------------
-------------------------------------------------------------------------------

  type stages is (S_MB, S_ADDR, S_NULL_CYCLE, S_WAIT_PTAG);

  signal data, digested                             : std_logic_vector (BLOCK_WIDTH-1 downto 0);
  signal input_length                               : std_logic_vector (BYTES_WIDTH-1 downto 0);
  signal start, started_hashing, hash_ready, load_k : std_logic;
  signal clearn                                     : std_logic;

  signal stage : stages;
begin

  assert C_SIPHASH_BLOCK_WIDTH = BLOCK_WIDTH
    --REPORT "PTAG-Generator parameter misconfigured." & integer'image(C_SIPHASH_BLOCK_WIDTH) SEVERITY FAILURE;
    report "PTAG-Generator parameter misconfigured." severity failure;

  HashComponent:
    siphash port map (
      m          => data,
      b          => input_length,
      rst_n      => clearn,
      clk        => clk,
      init       => start,
--      load_k     => load_k,
      key        => key,
      init_ready => started_hashing,
      hash_ready => hash_ready,
      hash       => digested
      );

  ptag          <= digested;
  -- input_data <= data WHEN stage /= S_ADDR ELSE STD_LOGIC_VECTOR(TO_UNSIGNED((BLOCK_WIDTH-C_ADDR_WIDTH)/8,64));
  input_data    <= data;
--  load_key_done <= load_k;
  clearn        <= rstn and enable_computation;

  SplitInput :
  process (clk, rstn, enable_computation)
    -- The counter has to wait hash_ready to start over.
    variable counter : natural range 0 to BLOCK_WIDTH-1;
	variable printed : boolean;
  begin
  --ready for comput can be used as enable
    if (rstn = '0') or (enable_computation = '0') then
      counter      := 0;
      input_length <= "0000";
      start        <= '0';
      load_k       <= '0';
      data         <= (others => '0');
      stage        <= S_MB;
      ptag_ready   <= '0';
    elsif (RISING_EDGE(clk)) then
      -- This pass the key in blocks. It should be 2 blocks.
      case stage is
--        when S_KEY =>
--          data         <= key ((BLOCK_WIDTH*(counter+1))-1 downto BLOCK_WIDTH*counter);
--          load_k       <= '1';
--          input_length <= "1000";
--          if counter >= C_KEY_LENGTH/BLOCK_WIDTH-1 then
--            stage   <= S_MB;
--            counter := 0;
--          else
--            counter := counter+1;
--          end if;
        when S_MB =>
          -- Now, the memory block will be passed over (C_LINE_SIZE*C_WORD_WIDTH)/BLOCK_WIDTH cycles.
          data         <= memory_block ((BLOCK_WIDTH*(counter+1))-1 downto BLOCK_WIDTH*counter);
--			debug_data		<= memory_block ((BLOCK_WIDTH*(counter+1))-1 downto BLOCK_WIDTH*counter);
			debug_data		<= std_logic_vector(to_unsigned(counter+1,BLOCK_WIDTH));
			increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_S_ADDR_DBUG));

          load_k       <= '0';
          input_length <= "1000";
          if counter = 0 then
            start <= '1';
          else
            start <= '0';
          end if;
          if counter >= (C_LINE_SIZE*C_WORD_WIDTH)/BLOCK_WIDTH -1 then
            stage   <= S_ADDR;
            counter := 0;
          else
            counter := counter+1;
          end if;
        -- Here we assumed that the address has at most the same length of block_width.
        when S_ADDR =>
			printed := false;
          if (C_ADDR_WIDTH < BLOCK_WIDTH) then
            data         <= (BLOCK_WIDTH - C_ADDR_WIDTH -1 downto 0 => '0') & address;
			debug_data		<= std_logic_vector(to_unsigned(to_integer(unsigned(debug_data))+1,BLOCK_WIDTH));
            input_length <= std_logic_vector(TO_UNSIGNED((BLOCK_WIDTH-C_ADDR_WIDTH)/8, BYTES_WIDTH));
            stage        <= S_WAIT_PTAG;
          else
            --TODO: augusto - check this line the size doesn't match
            data         <= (BLOCK_WIDTH - C_ADDR_WIDTH -1 downto 0 => '0') & address;--address;
            input_length <= "1000";
            stage        <= S_NULL_CYCLE;
          end if;
			debug_data		<= (BLOCK_WIDTH - C_ADDR_WIDTH -1 downto 0 => '0') & address;
			increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_S_ADDR_DBUG));

        -- Additional cycle for SipHash when last block has 8 bytes.
        when S_NULL_CYCLE =>
          input_length <= "0000";
          data         <= (others => '0');
          stage        <= S_WAIT_PTAG;
			debug_data		<= x"FFFFAAAAAAAAFFFF";
			increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_S_ADDR_DBUG));
        when S_WAIT_PTAG =>
			if not printed then
--		debug_data		<= x"AAAAFFFF0000" & log;
--		increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_S_ADDR_DBUG));
--		log				<= std_logic_vector(to_unsigned(to_integer(unsigned(log))+1, 16));
				printed := true;
			end if;
			debug_data		<= std_logic_vector(to_unsigned(to_integer(unsigned(debug_data))+1,BLOCK_WIDTH));
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_S_ADDR_DBUG));
          if hash_ready = '1' then
			if printed then
				debug_data		<= digested; 
				increment_addr	<= std_logic_vector(to_unsigned(to_integer(unsigned(increment_addr))+1,C_S_ADDR_DBUG));
				printed := false;
			end if;
            ptag_ready   <= '1';
            counter      := 0;
            data         <= (others => '0');
            input_length <= "0000";
            start        <= '0';
            --stage        <= S_KEY;
          end if;
      end case;
    end if;
  end process;

--COMMENTED--	debug_memory_inst : s_debug_memory PORT MAP (
--COMMENTED--			address	 => increment_addr,
--COMMENTED--			clock	 => clk,
--COMMENTED--			data	 => debug_data,
--COMMENTED--			wren	 => '1',
--COMMENTED--			q		 => open
--COMMENTED--		);

end architecture;

