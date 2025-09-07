# grid [![CI Status][ci-img]][ci] [![Hex.pm Version][hex-img]][hex] [![Minimum Erlang Version][erlang-img]][erlang] [![License][license-img]][license]

Erlang formatting library for tabular data.

The library can take most types of list with potential tabular data and format
it as a table. Rows can be maps, tuples, lists or proplists (or a mix of
these).

## Features

* **Sensible** 🧠

  Without any options, *grid* tries to do the sensible thing and format your
  data in the best possible tabular way.

* **Configurable** 🔧

  Want to re-order columns? Skip columns? Format rows or headers? *grid* has
  got your back!

* **Customizable** 🎨

  *grid* comes with a few built-in styles. You can also easily create your own
  custom style.

## Examples

### Raw Data

```erl
1> io:format(grid:format(application:which_applications())).
inets       INETS  CXC 138 49                        8.1
ssl         Erlang/OTP SSL application               10.8.5
public_key  Public key infrastructure                1.13.1
asn1        The Erlang ASN1 compiler version 5.0.21  5.0.21
crypto      CRYPTO                                   5.1.2
stdlib      ERTS  CXC 138 10                         4.1.1
kernel      ERTS  CXC 138 10                         8.5.1
```

### Custom Options

```erl
2> io:format(grid:format(application:which_applications(),#{header => titlecase, columns => [application, description, version], style => rounded})).
╭───────────┬──────────────────────────────────────┬───────╮
│Application│Description                           │Version│
╞═══════════╪══════════════════════════════════════╪═══════╡
│inets      │INETS  CXC 138 49                     │9.3.1  │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌┤
│ssl        │Erlang/OTP SSL application            │11.2.6 │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌┤
│public_key │Public key infrastructure             │1.17   │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌┤
│asn1       │The Erlang ASN1 compiler version 5.3.1│5.3.1  │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌┤
│crypto     │CRYPTO                                │5.5.2  │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌┤
│stdlib     │ERTS  CXC 138 10                      │6.2    │
├╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌┤
│kernel     │ERTS  CXC 138 10                      │10.2.1 │
╰───────────┴──────────────────────────────────────┴───────╯
```

### Proplists

```erl
3> io:format(grid:format([ets:info(T) || T <- ets:all()], #{header => titlecase})).
Compressed  Decentralized counters  Heir  Id                                   Keypos  Memory  Name                              Named table  Node           Owner      Protection  Read concurrency  Size  Type         Write concurrency
false       false                   none  #Ref<0.2124221214.1451884545.77687>  1       147     shell_records                     false        nonode@nohost  <0.215.0>  public      false             0     ordered_set  false
false       false                   none  #Ref<0.2124221214.1451884548.77475>  1       6815    logger                            true         nonode@nohost  <0.42.0>   protected   true              4     set          true
false       false                   none  #Ref<0.2124221214.1451884548.77482>  1       3073    ac_tab                            true         nonode@nohost  <0.44.0>   public      true              35    set          false
false       false                   none  #Ref<0.2124221214.1451884548.77521>  1       48976   code                              false        nonode@nohost  <0.50.0>   private     false             557   set          false
false       false                   none  #Ref<0.2124221214.1451884548.77522>  1       8838    code_names                        true         nonode@nohost  <0.50.0>   public      false             57    set          false
false       false                   none  #Ref<0.2124221214.1451884548.77533>  1       631     inet_db                           true         nonode@nohost  <0.51.0>   public      false             30    set          false
false       false                   none  #Ref<0.2124221214.1451884548.77534>  9       313     inet_cache                        true         nonode@nohost  <0.51.0>   public      false             0     bag          false
false       false                   none  #Ref<0.2124221214.1451884548.77535>  1       313     inet_hosts_byname                 true         nonode@nohost  <0.51.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77536>  1       313     inet_hosts_byaddr                 true         nonode@nohost  <0.51.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77537>  1       313     inet_hosts_file_byname            true         nonode@nohost  <0.51.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77538>  1       313     inet_hosts_file_byaddr            true         nonode@nohost  <0.51.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77539>  1       313     inet_sockets                      true         nonode@nohost  <0.51.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77560>  1       393     rex_nodes_observer                false        nonode@nohost  <0.53.0>   protected   true              0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77567>  1       313     global_locks                      true         nonode@nohost  <0.54.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77568>  1       393     global_names                      true         nonode@nohost  <0.54.0>   protected   true              0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77569>  1       313     global_names_ext                  true         nonode@nohost  <0.54.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77570>  1       313     global_pid_names                  true         nonode@nohost  <0.54.0>   protected   false             0     bag          false
false       false                   none  #Ref<0.2124221214.1451884548.77571>  1       313     global_pid_ids                    true         nonode@nohost  <0.54.0>   protected   false             0     bag          false
false       false                   none  #Ref<0.2124221214.1451884548.77572>  1       313     global_lost_connections           true         nonode@nohost  <0.54.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77573>  1       313     global_node_resources             true         nonode@nohost  <0.54.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77587>  1       313     file_io_servers                   true         nonode@nohost  <0.59.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77722>  1       313     ssl_pem_cache                     true         nonode@nohost  <0.88.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77730>  1       147     client_ssl_otp_session_cache      false        nonode@nohost  <0.89.0>   protected   false             0     ordered_set  false
false       false                   none  #Ref<0.2124221214.1451884548.77731>  1       313     ssl_otp_cacertificate_db          false        nonode@nohost  <0.89.0>   public      false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77732>  1       313     ssl_otp_ca_file_ref               false        nonode@nohost  <0.89.0>   public      false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77733>  1       313     ssl_otp_ca_ref_file_mapping       false        nonode@nohost  <0.89.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77734>  1       313     ssl_otp_crl_cache                 false        nonode@nohost  <0.89.0>   protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77735>  1       313     ssl_otp_crl_issuer_mapping        false        nonode@nohost  <0.89.0>   protected   false             0     bag          false
false       false                   none  #Ref<0.2124221214.1451884548.77764>  1       313     dtls_listener_sup                 true         nonode@nohost  <0.102.0>  public      false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77786>  2       313     httpc_manager__session_db         true         nonode@nohost  <0.110.0>  public      false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77787>  1       313     httpc_manager__handler_db         true         nonode@nohost  <0.110.0>  protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77790>  2       313     httpc_manager__session_cookie_db  false        nonode@nohost  <0.110.0>  protected   false             0     bag          false
false       false                   none  #Ref<0.2124221214.1451884548.77801>  2       313     httpc_rebar__session_db           true         nonode@nohost  <0.113.0>  public      false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77802>  1       313     httpc_rebar__handler_db           true         nonode@nohost  <0.113.0>  protected   false             0     set          false
false       false                   none  #Ref<0.2124221214.1451884548.77803>  2       313     httpc_rebar__session_cookie_db    false        nonode@nohost  <0.113.0>  protected   false             0     bag          false
false       false                   none  #Ref<0.2124221214.1451884548.78098>  1       320     disk_log_names                    true         nonode@nohost  <0.126.0>  protected   false             1     set          false
false       false                   none  #Ref<0.2124221214.1451884548.78099>  1       320     disk_log_pids                     true         nonode@nohost  <0.126.0>  protected   false             1     set          false
false       false                   none  #Ref<0.2124221214.1451884548.78107>  2       41409   package_index                     true         nonode@nohost  <0.9.0>    public      false             576   ordered_set  false
```

### Mixed Data

```erl
4> io:format(grid:format([#{type => 1},[2,foo,"c"],{3, <<"quux">>}])).
1
2  foo   c
3  quux
```

## Styles

* `default`
  ```
  Animal Count
  Dog    3
  Cat    5
  ```
* `simple`
  ```
  ┌──────┬─────┐
  │Animal│Count│
  ├──────┼─────┤
  │Dog   │3    │
  │Cat   │5    │
  └──────┴─────┘
  ```
* TODO: Show all styles

[ci]:          https://github.com/eproxus/grid/actions/workflows/continous_integration.yaml?query=branch%3Amain
[ci-img]:      https://img.shields.io/github/actions/workflow/status/eproxus/grid/continous_integration.yaml?label=ci
[hex]:         https://hex.pm/packages/grid
[hex-img]:     https://img.shields.io/hexpm/v/grid
[license]:     LICENSE.md
[license-img]: https://img.shields.io/hexpm/l/grid
[erlang]:      https://github.com/eproxus/grid/blob/main/.github/workflows/continous_integration.yaml#L12
[erlang-img]:  https://img.shields.io/badge/erlang-25+-blue.svg
