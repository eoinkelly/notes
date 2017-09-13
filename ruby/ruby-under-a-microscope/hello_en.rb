puts "Hello, World!"

# ruby --dump parsetree hello_en.rb
###########################################################
## Do NOT use this node dump for any purpose other than  ##
## debug and research.  Compatibility is not guaranteed. ##
###########################################################

# @ NODE_SCOPE (line: 1)
# +- nd_tbl: (empty)
# +- nd_args:
# |   (null node)
# +- nd_body:
#     @ NODE_PRELUDE (line: 1)
#     +- nd_head:
#     |   (null node)
#     +- nd_body:
#     |   @ NODE_FCALL (line: 1)
#     |   +- nd_mid: :puts
#     |   +- nd_args:
#     |       @ NODE_ARRAY (line: 1)
#     |       +- nd_alen: 1
#     |       +- nd_head:
#     |       |   @ NODE_STR (line: 1)
#     |       |   +- nd_lit: "Hello, World!"
#     |       +- nd_next:
#     |           (null node)
#     +- nd_compile_option:
#         +- coverage_enabled: true
