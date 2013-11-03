all : exp csb exp.trans csb.trans

cfg.cmx : cfg.ml
	ocamlopt -c cfg.ml

compile_bootstrap.cmx : compile_bootstrap.ml cfg.cmx
	ocamlopt -c compile_bootstrap.ml

compile_bootstrap : compile_bootstrap.cmx cfg.cmx
	ocamlopt -o compile_bootstrap cfg.cmx compile_bootstrap.cmx

cfg.trans : compile_bootstrap
	compile_bootstrap

exp.cmx : exp.ml cfg.cmx
	ocamlopt -c exp.ml

csb.cmx : csb.ml cfg.cmx exp.cmx
	ocamlopt -c csb.ml

exp : exp.cmx cfg.cmx exp.trans
	ocamlopt -o exp cfg.cmx exp.cmx

csb : csb.cmx cfg.cmx exp.cmx csb.trans
	ocamlopt -o csb cfg.cmx exp.cmx csb.cmx

compile_cfg.cmx : compile_cfg.ml cfg.cmx
	ocamlopt -c compile_cfg.ml

compile_cfg : compile_cfg.cmx cfg.cmx
	ocamlopt -o compile_cfg cfg.cmx compile_cfg.cmx

exp.trans : compile_cfg exp.cfg cfg.trans
	compile_cfg exp.cfg exp.trans

csb.trans : compile_cfg csb.cfg cfg.trans
	compile_cfg csb.cfg csb.trans

clean :
	rm *.cmi *.cmx *.trans *.o compile_bootstrap compile_cfg csb exp