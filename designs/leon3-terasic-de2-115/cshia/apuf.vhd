library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library altera;
use altera.altera_primitives_components.all;
library CSHIA;
use CSHIA.apuf_pkg.all;

entity apuf is
	generic (
		n_stages		: integer := 64;
		n_pufs			: integer := 64
	);
	port (
		clk,
		rst_n,
		clearn			: in std_logic;
		challenge		: in std_logic_vector (n_stages-1 downto 0);
		responses		: out std_logic_vector (n_pufs-1 downto 0)
	);
	end entity;

	architecture rtl of apuf is
		attribute keep						 		: string;
		
		signal ff_clks, ff_ds						: std_logic_vector (n_pufs-1 downto 0);

		signal	clk1, clk2							: std_logic;
		
		attribute keep of	ff_clks, ff_ds,
							clk1, clk2				: signal is "true";
	begin
		puf_generations:
		for i in 0 to n_pufs-1 generate
			pufs: apuf_unit generic map (
				n_stages	=> N_STAGES
			)
			port map(
				in1				=> clk1,
				in2				=> clk2,
				challenge		=> challenge,
				out1			=> ff_ds(i),
				out2			=> ff_clks(i)
			);
		end generate;
		
	   -- fdre: single data rate d flip-flop with synchronous reset and
	   --       clock enable (posedge clk).  
	   --       spartan-6
	   -- xilinx hdl language template, version 14.7
	   
		flipflop_generation:
		for i in 0 to n_pufs-1 generate
	--		fdre_inst : fdre
	--		generic map (
	--			init	=> '0'				-- initial value of register ('0' or '1')  
	--		) port map (
	--			q		=> responses(i),	-- data output
	--			c		=> ff_clks(i),		-- clock input
	--			ce		=> rst_n,			-- clock enable input
	--			r		=> clear,			-- synchronous reset input
	--			d		=> ff_ds(i)			-- data input
	--		);
			dff_inst : DFF
			port map (
				d			=> ff_ds(i),
				clk			=> ff_clks(i), 
				clrn		=> clearn,
				prn			=> rst_n,
				q			=> responses(i)
			);
	end generate;

	clock_divisor:
	process (clk, rst_n)
	begin
		if (rst_n = '0') then
			clk1			<= '0';
			clk2			<= '0';
		elsif (rising_edge(clk)) then
			clk1			<= not clk1;
			clk2			<= not clk2;
		end if;
	end process;
end architecture;
