```
functions from the snippet

1. i_func_info_IaaI/4
1. i_select_val2_rfccff/6
3. move_return_cr/2
4. move_return_nr/2
5. move_rx/2
6. move_cr/2
7. allocate_tt/2
8. call_bif_e/1
9. deallocate_return_Q/1
```

```
# MEM_ADDRESS:  FUNC_NAME ARG1 ARG2 ...
00000000129C0140: i_func_info_IaaI 0 'Elixir.Simple' '__info__' 1
00000000129C0168: i_select_val2_rfccff x(0) f(00000000129C01B0) functions macros f(00000000129C0198) f(00000000129C01A8)
00000000129C0198: move_return_cr [{greet,0}] x(0)
00000000129C01A8: move_return_nr [] x(0)
00000000129C01B0: move_rx x(0) x(1)
00000000129C01C0: move_cr 'Elixir.Simple' x(0)
00000000129C01D0: allocate_tt 0 2
00000000129C01E0: call_bif_e erlang:get_module_info/2
00000000129C01F0: deallocate_return_Q 0

00000000129C0200: i_func_info_IaaI 0 'Elixir.Simple' greet 0
00000000129C0228: move_return_cr <<"Hello there">> x(0)

00000000129C0238: i_func_info_IaaI 0 'Elixir.Simple' module_info 0
00000000129C0260: move_cr 'Elixir.Simple' x(0)
00000000129C0270: allocate_tt 0 1
00000000129C0280: call_bif_e erlang:get_module_info/1
00000000129C0290: deallocate_return_Q 0

00000000129C02A0: i_func_info_IaaI 0 'Elixir.Simple' module_info 1
00000000129C02C8: move_rx x(0) x(1)
00000000129C02D8: move_cr 'Elixir.Simple' x(0)
00000000129C02E8: allocate_tt 0 2
00000000129C02F8: call_bif_e erlang:get_module_info/2
00000000129C0308: deallocate_return_Q 0
```
