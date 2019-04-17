PROJECT = aiwx
PROJECT_DESCRIPTION = wechat tools for productions from ailink.io
PROJECT_VERSION = 0.1.0

ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard -DENABLE_LOG

DEPS =  cowboy jsx ailib aihttp
dep_cowboy_commit = 2.6.1
dep_jsx_commit = v2.9.0
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.3.4
dep_aihttp = git https://github.com/DavidAlphaFox/aihttp.git tag-0.2.2

include erlang.mk
