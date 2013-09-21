# See LICENSE for licensing information.

PROJECT = alogger

DIALYZER = dialyzer
REBAR = ./rebar

all: app

# Application.

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

# Dialyzer.

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions #-Wunmatched_returns -Wunderspecs
