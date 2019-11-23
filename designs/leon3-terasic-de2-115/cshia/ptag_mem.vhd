library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library CSHIA;
use CSHIA.ptag_memory_pkg.all;
use CSHIA.tree_pkg.all;
use CSHIA.handler_pkg.all;

entity ptag_mem is
	PORT
	(
		address		: IN T_PTAG_ADDRESS;
		clock		: IN STD_LOGIC  := '1';
		data		: IN STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0);
		wren		: IN STD_LOGIC ;
		q			: OUT STD_LOGIC_VECTOR (C_PTAG_WIDTH-1 DOWNTO 0)
	);
end entity;

architecture rtl of ptag_mem is
	signal	wr_base,
			wr_tree			: std_logic;

	signal	addr_base		: T_PTAG_ADDR_BASE;
	signal	addr_tree		: T_PTAG_ADDR_TREE;

	signal	output1,
			output2			: std_logic_vector (C_PTAG_WIDTH-1 DOWNTO 0);

begin

-------------------------------------------------------------------------------
-- Verify constants 
-------------------------------------------------------------------------------
	assert	C_ADDR_BASE + TC_LOG2_DEPTH + 1 <= C_PTAG_ADDR
	    report "Logical PTAG address too small." severity failure;
-------------------------------------------------------------------------------
-- Decoding logic
-------------------------------------------------------------------------------

	wr_base		<= (not address(C_PTAG_ADDR-1)) and wren;
	wr_tree		<= address(C_PTAG_ADDR-1) and wren;

	process (address)
--		constant	C_BASE_ADDR		: T_PTAG_ADDR_TREE := std_logic_vector(shift_right(unsigned(C_DATA_SEG_STARTING_ADDR), C_BASE_ADDR_BITS_W))(C_ADDR_TREE-1 downto 0);

		variable	shift			: natural;
		variable	tmp_addr		: T_PTAG_ADDR_TREE;
		variable	aux_addr		: T_PTAG_ADDR_TREE;
	begin
		if address(C_PTAG_ADDR-1) = '1' then
			shift			:= to_integer(unsigned(address(C_PTAG_ADDR-2 downto C_PTAG_ADDR-TC_LOG2_DEPTH-1)));
--			aux_addr		:= std_logic_vector(unsigned(address(C_ADDR_TREE-1 downto 0)) - shift_right(unsigned(C_BASE_ADDR), shift*TC_LOG2_CHUNK));
			aux_addr		:= address(C_ADDR_TREE-1 downto 0);
			tmp_addr		:= std_logic_vector(to_unsigned(1, C_ADDR_TREE)); 
			tmp_addr		:= std_logic_vector(shift_left(unsigned(tmp_addr),C_ADDR_TREE-shift));
			addr_tree		<= tmp_addr or aux_addr; 
			addr_base		<= (others => '0');
		else
			addr_base		<= address(C_ADDR_BASE-1 downto 0);
			addr_tree		<= (others => '0');
		end if;
	end process;

	ptag_mem_base_inst : ptag_mem_base PORT MAP (
		address	 => addr_base,
		clock	 => clock,
		data	 => data,
		wren	 => wr_base,
		q		 => output1 
	);

	ptag_mem_tree_inst : ptag_mem_tree PORT MAP (
		address	 => addr_tree,
		clock	 => clock,
		data	 => data,
		wren	 => wr_tree,
		q		 => output2 
	);

	q	<= output1 when address(C_PTAG_ADDR-1) = '0' else output2;
end architecture;

