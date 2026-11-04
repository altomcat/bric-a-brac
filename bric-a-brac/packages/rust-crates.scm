
(define-module (bric-a-brac packages rust-crates)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs
            my-cargo-inputs))

;;;
;;; This file is managed by guix import.  DO NOT add definitions manually.
;;;

(define* (crate-name->package-name name)
  (downstream-package-name "rust-" name))

(define* (crate-source name version hash #:key (patches '()) (snippet #f))
  (origin
    (method url-fetch)
    (uri (crate-uri name version))
    (file-name
     (string-append (crate-name->package-name name) "-" version ".tar.gz"))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (patches patches)
    (snippet snippet)))

(define-syntax define-cargo-inputs
  (syntax-rules (=>)
    ((_ lookup inputs ...)
     (define lookup
       (let ((table (make-hash-table)))
         (letrec-syntax ((record
                          (syntax-rules (=>)
                            ((_) #t)
                            ((_ (name => lst) rest (... ...))
                             (begin
                               (hashq-set! table 'name (filter identity lst))
                               (record rest (... ...)))))))
           (record inputs ...)
           (lambda (name)
             "Return the inputs for NAME."
             (hashq-ref table name))))))))

(define* (my-cargo-inputs name
                          ;; NOTE: Change to your own module.
                          #:key (module '(bric-a-brac packages rust-crates)))
  "Lookup Cargo inputs for NAME defined in MODULE, return an empty list if
unavailable."
  (let ((lookup (module-ref (resolve-interface module) 'lookup-cargo-inputs)))
    (or (lookup name)
        (begin
          (warning (G_ "no Cargo inputs available for '~a'~%") name)
          '()))))

;;;
;;; Rust dependencies fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)


(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aho-corasick-1.1.5
  (crate-source "aho-corasick" "1.1.5"
                "1fhjkp2nbs7gg4y1b68hpc8028rpax8aiscfh9b60q78m4pn90n9"))

(define rust-android-system-properties-0.1.6
  (crate-source "android_system_properties" "0.1.6"
                "1g3z4ga15a9022vbgi31qqyb7pgk23saq7xfarn6yslpr54ic8mf"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-anstream-1.0.0
  (crate-source "anstream" "1.0.0"
                "13d2bj0xfg012s4rmq44zc8zgy1q8k9yp7yhvfnarscnmwpj2jl2"))

(define rust-anstyle-1.0.14
  (crate-source "anstyle" "1.0.14"
                "0030szmgj51fxkic1hpakxxgappxzwm6m154a3gfml83lq63l2wl"))

(define rust-anstyle-parse-1.0.0
  (crate-source "anstyle-parse" "1.0.0"
                "03hkv2690s0crssbnmfkr76kw1k7ah2i6s5amdy9yca2n8w7zkjj"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.104
  (crate-source "anyhow" "1.0.104"
                "0w34jjcm02p5g9kvsjr1dvpw0zs2fi7igi6nr414fkm5gz85w2ik"))

(define rust-ar-archive-writer-0.5.3
  (crate-source "ar_archive_writer" "0.5.3"
                "1dhjwdapx0ydx38r7aj8w4rqmdhxs1xl2zp8xala0h11zzg5ikbk"))

(define rust-arrayvec-0.7.8
  (crate-source "arrayvec" "0.7.8"
                "0mmd8lrijbvg1qp4c5zis5dq41a3mjv2rb6bxkyj9kwaw2k6gyyk"))

(define rust-autocfg-1.5.1
  (crate-source "autocfg" "1.5.1"
                "0lqasy5i30flcgih1b50kvsk6z32g09r1q4ql7q81pj6228jy0zj"))

(define rust-bincode-2.0.1
  (crate-source "bincode" "2.0.1"
                "0h5pxp3dqkigjwy926a03sl69n9wv7aq4142a20kw9lhn3bzbsin"))

(define rust-bincode-derive-2.0.1
  (crate-source "bincode_derive" "2.0.1"
                "029wmh26hq3hhs1gq629y0frn2pkl7ld061rk23fji8g8jd715dz"))

(define rust-bitflags-2.13.2
  (crate-source "bitflags" "2.13.2"
                "01hbgjwvid66850fzi76mvn5f2bqycx6sf165ng1kfjqq9bl1v9x"))

(define rust-blake3-1.8.7
  (crate-source "blake3" "1.8.7"
                "1bi7m95sggkkcmlyiykflbb37v84cgzkm283r1yrfxhzq57lb7kd"))

(define rust-bumpalo-3.20.3
  (crate-source "bumpalo" "3.20.3"
                "0jc6va3nwcqikm7chnpdv1s87my3gs2j7g1sc7g3k91brg3arxbj"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-cc-1.11.0
  (crate-source "cc" "1.11.0"
                "0ssz5frnjb3x873yhp2790xf9v8jz8hdbgj28i1gm861rvg8jn9p"))

(define rust-cfg-if-1.0.5
  (crate-source "cfg-if" "1.0.5"
                "0026j56901nzjraap3da0a8njw42j66zcxnn6s2s9aa5bcblhxjf"))

(define rust-chrono-0.4.45
  (crate-source "chrono" "0.4.45"
                "09rkcgk6is2sdhqs9142zv8xqnj8ryx8m9hknllqwyv9wxi9x9qs"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-clap-4.6.7
  (crate-source "clap" "4.6.7"
                "0il98y2rfw75984ck59znd4n592p07bxz8yy3a9blddb02rpd25a"))

(define rust-clap-builder-4.6.7
  (crate-source "clap_builder" "4.6.7"
                "0kbhai5rv1vj9r4np52g2b9fmhvy3y82digs9v40c57bgbxrf1zc"))

(define rust-clap-derive-4.6.7
  (crate-source "clap_derive" "4.6.7"
                "1q36bgbkfb9rdz7jcvd0ffv0wwlyw0lg3z6iwdcybm0mjjvm3izr"))

(define rust-clap-lex-1.1.1
  (crate-source "clap_lex" "1.1.1"
                "11k4m4ngn39iz39lnq6nn6kyxzp62mnm1d86qfad3q0vlk33n4qw"))

(define rust-colorchoice-1.0.5
  (crate-source "colorchoice" "1.0.5"
                "0w75k89hw39p0mnnhlrwr23q50rza1yjki44qvh2mgrnj065a1qx"))

(define rust-constant-time-eq-0.4.2
  (crate-source "constant_time_eq" "0.4.2"
                "16zamq60dq80k3rqlzh9j9cpjhishmh924lnwbplgrnmkkvfylix"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-cpufeatures-0.3.1
  (crate-source "cpufeatures" "0.3.1"
                "0rkm1l35jy1z1yfg723fddsxc7vr0gc1hhfvc138hnqiwc58p8jw"))

(define rust-crc32fast-1.5.4
  (crate-source "crc32fast" "1.5.4"
                "017g8wq37261s7i21v2p7wacl8pr3p4why0d9dfd2dskdm0x75sz"))

(define rust-criterion-0.7.0
  (crate-source "criterion" "0.7.0"
                "0a6rrgka8013l5d7bhlln60fja7nj4qmlhc4z92y3hqc5fk4gh71"))

(define rust-criterion-plot-0.6.0
  (crate-source "criterion-plot" "0.6.0"
                "0f73hgvmxacda8xk3a2xfbrqmphcz1ail2xdhjfmkbnzqw6wq6wv"))

(define rust-crossbeam-deque-0.8.8
  (crate-source "crossbeam-deque" "0.8.8"
                "06kip6ay8wcx5y4flg6wxbnyd44ay8308c8lf8y3iglh6v3kybv2"))

(define rust-crossbeam-epoch-0.9.21
  (crate-source "crossbeam-epoch" "0.9.21"
                "17bdp2linl0milbmx00s3bda3fphgc85im1gqwa3p6hhhw39hx6w"))

(define rust-crossbeam-utils-0.8.23
  (crate-source "crossbeam-utils" "0.8.23"
                "1ilan2nw7fvka8hki80fr57a5dgd4mvcsvwq60437j6yvlwyw7m3"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-csv-1.4.0
  (crate-source "csv" "1.4.0"
                "0f7r2ip0rbi7k377c3xmsh9xd69sillffhpfmbgnvz3yrxl9vkaj"))

(define rust-csv-core-0.1.13
  (crate-source "csv-core" "0.1.13"
                "10lppd3fdb1i5npgx9xqjs5mjmy2qbdi8n16i48lg03ak4k3qjkh"))

(define rust-dyf-0.1.4
  (crate-source "dyf" "0.1.4"
                "0qrxz96755xlix6zqzb3cxs5h1pxjfrb385l3a875rc8565lb000"))

(define rust-either-1.19.0
  (crate-source "either" "1.19.0"
                "1gjq21g0sgk5ylpj85zafcinwhh3jj91i6drhb4278vw2v17370f"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-fastrand-2.5.0
  (crate-source "fastrand" "2.5.0"
                "08q2r30y62winysimnlpbvw9kiwn0rmdlidqlmzd6z90mv764z6s"))

(define rust-filetime-0.2.29
  (crate-source "filetime" "0.2.29"
                "0napyyfccb26r7fyh9hg7ixrh4vph9h7y7k4iv1j19phqwrpla2w"))

(define rust-find-msvc-tools-0.1.15
  (crate-source "find-msvc-tools" "0.1.15"
                "0j5sh4lb7g8xh6kd67s0vz037ah5hlp9f2rbgbg9f41rqmf92rhw"))

(define rust-flagset-0.4.7
  (crate-source "flagset" "0.4.7"
                "1zplx30g76kl5la3ayl9ns5p3diqd9zphbcggqcm4nm7411q5b5p"))

(define rust-flate2-1.1.10
  (crate-source "flate2" "1.1.10"
                "1jvd2cl8j5hyf8imi62y1x7gwzz1hajirni0801yxhds1qp4wqvf"))

(define rust-fs-walk-0.2.2
  (crate-source "fs-walk" "0.2.2"
                "03ljrjzk3lkbh7lpl0sqiydc0nsiyw4h5fvmhbjqxk63gdakasbl"))

(define rust-futures-core-0.3.34
  (crate-source "futures-core" "0.3.34"
                "0pjgv4fx0np6hrs5sz5a2phabwv0z70yr51v03injbi44bjrkmlj"))

(define rust-futures-task-0.3.34
  (crate-source "futures-task" "0.3.34"
                "1zfilqs8nwlfqz4prk7ihvpp5avvzins87ibzlxzq5fhs7ipshfd"))

(define rust-futures-util-0.3.34
  (crate-source "futures-util" "0.3.34"
                "1g3r9ghzq7c2fh34lis43i72xavk9p84npgfwgb5vfpqcwjajl0d"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.3
  (crate-source "getrandom" "0.4.3"
                "16b0202fkdwz3p2cyll82dv24ljbn0wiyy829v4lwbkbflyqh3ih"))

(define rust-half-2.7.1
  (crate-source "half" "2.7.1"
                "0jyq42xfa6sghc397mx84av7fayd4xfxr4jahsqv90lmjr5xi8kf"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-iana-time-zone-0.1.65
  (crate-source "iana-time-zone" "0.1.65"
                "0w64khw5p8s4nzwcf36bwnsmqzf61vpwk9ca1920x82bk6nwj6z3"))

(define rust-iana-time-zone-haiku-0.1.2
  (crate-source "iana-time-zone-haiku" "0.1.2"
                "17r6jmj31chn7xs9698r122mapq85mfnv98bb4pg6spm0si2f67k"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itoa-1.0.18
  (crate-source "itoa" "1.0.18"
                "10jnd1vpfkb8kj38rlkn2a6k02afvj3qmw054dfpzagrpl6achlg"))

(define rust-js-sys-0.3.107
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.107"
                "0fjdb084avbdxqb6n14lzszwn91wg88npasfym7bz2q9ahhrvh4d"))

(define rust-lazy-static-1.5.1
  (crate-source "lazy_static" "1.5.1"
                "1yqaqmp510xw2ldpw88mx9b5s5qj8flb4rw0wd9ks1zpk9j0z1r0"))

(define rust-libc-0.2.191
  (crate-source "libc" "0.2.191"
                "02i4j7pjh129y92rijaia4576131fa2xjsii1dc3c1fnlv4qgkb5"))

(define rust-linux-raw-sys-0.12.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-log-0.4.35
  (crate-source "log" "0.4.35"
                "0p002750b2hab9bnlxxrvk5g9dvyaf23kiy39s6wsi6s29nzh5ax"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-memchr-2.8.3
  (crate-source "memchr" "2.8.3"
                "161xa63ipfanf8v3nb82xd5hqgydv55nzw59wyngqbz6alfaz2yg"))

(define rust-memmap2-0.9.12
  (crate-source "memmap2" "0.9.12"
                "0my158ms3h2akj1l8bzabwjz1v9jkw8klcg82am2bk4yb8as233b"))

(define rust-miniz-oxide-0.9.1
  (crate-source "miniz_oxide" "0.9.1"
                "0k2bgjzk2sbsynpsv4wizwxbqp6vs7g08y5anbkrh3l6a15bqgxn"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-object-0.39.1
  (crate-source "object" "0.39.1"
                "16vkcaamik55jd9f04g73hvgsm5w636gb4w06x3nafvsih4nqnif"))

(define rust-once-cell-1.21.4
  (crate-source "once_cell" "1.21.4"
                "0l1v676wf71kjg2khch4dphwh1jp3291ffiymr2mvy1kxd5kwz4z"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-pest-2.9.5
  (crate-source "pest" "2.9.5"
                "0q3nkmv7947j750m63adihzynqn8mn6g096rdsriq0066lapbnx8"))

(define rust-pest-derive-2.9.5
  (crate-source "pest_derive" "2.9.5"
                "1d1kv4khnw50xgb57v5n679618spmjps052jgnv68mmk3zhigai0"))

(define rust-pest-generator-2.9.5
  (crate-source "pest_generator" "2.9.5"
                "17gyswbjnzdh1z5sz4fq6jzv8mdv21jk2cmjnbzcnk7lrjpwavk2"))

(define rust-pest-meta-2.9.5
  (crate-source "pest_meta" "2.9.5"
                "0p8gf9lm82b887s7n2il8r0rja95khala9w3hs0v1hb5m9n5px5k"))

(define rust-pest-stack-guard-1.0.1
  (crate-source "pest_stack_guard" "1.0.1"
                "0f9lff1gsw9rrvl87j34kajrqgrhdz15jb2kh85qz2s1si7ibils"))

(define rust-pin-project-lite-0.2.17
  (crate-source "pin-project-lite" "0.2.17"
                "1kfmwvs271si96zay4mm8887v5khw0c27jc9srw1a75ykvgj54x8"))

(define rust-plotters-0.3.7
  (crate-source "plotters" "0.3.7"
                "0ixpy9svpmr2rkzkxvvdpysjjky4gw104d73n7pi2jbs7m06zsss"))

(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))

(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))

(define rust-portable-atomic-1.15.0
  (crate-source "portable-atomic" "1.15.0"
                "11csag858ndk5w4yz17h91vy53ynh67r2903gwwdn2cnilzbdj05"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-proc-macro2-1.0.107
  (crate-source "proc-macro2" "1.0.107"
                "1nb6ly8kp65f724kj73ippc7lvydss24sm2vagk6qpklpg4pwplq"))

(define rust-psm-0.1.32
  (crate-source "psm" "0.1.32"
                "08m67yndaikwq8kviipphi50lfb25ph7j3gp4w3rffz6k52h7kad"))

(define rust-pyo3-0.29.3
  (crate-source "pyo3" "0.29.3"
                "0m8n3wcwiz33mghwxjkny4b6ji1g1rc16zpx46svjwvv4vx1h3bh"))

(define rust-pyo3-build-config-0.29.3
  (crate-source "pyo3-build-config" "0.29.3"
                "1nws96j6j37mja0fq8jfhnv6x77mwawxyfgyf47b2swgs32c0gvv"))

(define rust-pyo3-ffi-0.29.3
  (crate-source "pyo3-ffi" "0.29.3"
                "0c43d7564azpkhz13sv70pil7wd2qaq7p5yghrqav6pjkphvih6z"))

(define rust-pyo3-macros-0.29.3
  (crate-source "pyo3-macros" "0.29.3"
                "1ilxni4r14kb08qlr92kqncj82qshmj861zdlpx3m9gbnnjfh031"))

(define rust-pyo3-macros-backend-0.29.3
  (crate-source "pyo3-macros-backend" "0.29.3"
                "1cxxn73jgwp68cpxfifp515a4n38v16gy9vigdr5laz82rx8fhv1"))

(define rust-quote-1.0.47
  (crate-source "quote" "1.0.47"
                "00ch0yyzvv6s671ik0kcsbw8nigdaj2g3fr61kcahwx48aqlvgqz"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

(define rust-rand-0.9.5
  (crate-source "rand" "0.9.5"
                "0hbvllk8g28mqjld6hqmckk69w296qpzg95whm3didsyg46ivvxr"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-rayon-1.12.0
  (crate-source "rayon" "1.12.0"
                "0vcj63xgnk72c30vdrak7dhl53snnaqv9x2faf1d94hzg1kb2fgv"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-regex-1.13.1
  (crate-source "regex" "1.13.1"
                "1391a0a4100ik8cp7l577p3ip3haqq03rd9c5vdr7vcfdixj687h"))

(define rust-regex-automata-0.4.18
  (crate-source "regex-automata" "0.4.18"
                "1cml0rm0ssqfkibh9nh3gy4b6hbsbicj1rihpwf2a4v4nawm71dd"))

(define rust-regex-syntax-0.8.11
  (crate-source "regex-syntax" "0.8.11"
                "1m25h5q2wp976fb9gc3dsc9l99svcvd5cri8lncb51c46ydgzxnn"))

(define rust-rustix-1.1.6
  (crate-source "rustix" "1.1.6"
                "1ajqv2zxmfms30hhvp5bdzpixn5xqnz7s871vpa56h99wm16grz5"))

(define rust-rustversion-1.0.23
  (crate-source "rustversion" "1.0.23"
                "07z2a843fs80fawwflj9jwn49k9b0bd0dhhbvy0ar69vaxd72m6g"))

(define rust-ryu-1.0.23
  (crate-source "ryu" "1.0.23"
                "0zs70sg00l2fb9jwrf6cbkdyscjs53anrvai2hf7npyyfi5blx4p"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-serde-1.0.229
  (crate-source "serde" "1.0.229"
                "1fp04fq4a79bpm61xz1zy0pbz4kpc7d771zii1k3inmszq55jj21"))

(define rust-serde-core-1.0.229
  (crate-source "serde_core" "1.0.229"
                "0j1ajiha76h3nmd976il9li6975k121xa7jb39ws8n0yqp4s5p37"))

(define rust-serde-derive-1.0.229
  (crate-source "serde_derive" "1.0.229"
                "0j4k63i7h1bikxwz2c89ig0hrwbnl9mz1czn85xx99x5cc9dg9g7"))

(define rust-serde-json-1.0.154
  (crate-source "serde_json" "1.0.154"
                "1mjby21g8fxhrsfp73yz8wg6n2f4h23ah0nczds409l53f5wrsg7"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shlex-2.0.1
  (crate-source "shlex" "2.0.1"
                "1fjsll1cd7d2bcpdij9kd6w62rpbc7qqzvydvs021vsmr1cxvypq"))

(define rust-simd-adler32-0.3.10
  (crate-source "simd-adler32" "0.3.10"
                "1sny4y2qa5mwyxx5x59ln2p02vsdh92004njlslnx98imjc9489s"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-smallvec-1.16.4
  (crate-source "smallvec" "1.16.4"
                "1j772prjisi2872lxry7d9fkss3dfbjwl16jmsvvp2qvk2qx9mfh"))

(define rust-stacker-0.1.25
  (crate-source "stacker" "0.1.25"
                "0rwrws4iyh8cay7pghzf2dy55pprwfn1vm805f5czfh6cza4jzvh"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-syn-2.0.119
  (crate-source "syn" "2.0.119"
                "15vjy620l91a3q4n4f4gzhnflmdr6pnm38v2m6cpk86i8av32a47"))

(define rust-syn-3.0.9
  (crate-source "syn" "3.0.9"
                "0fw28lhl90kls24q2h2sp39yjb0lsvz5cwh9fd3f3w3v9kp8v36p"))

(define rust-tar-0.4.46
  (crate-source "tar" "0.4.46"
                "0h68bc0y1nma3h2ypj28vxc84msjydlrj8rviqwphg00lvcj2qiz"))

(define rust-target-lexicon-0.13.5
  (crate-source "target-lexicon" "0.13.5"
                "1jm6lmf9hsn7ri2d6v9gg6fy24lylhskh6pbxh71f82wdxd97dmd"))

(define rust-tempfile-3.27.0
  (crate-source "tempfile" "3.27.0"
                "1gblhnyfjsbg9wjg194n89wrzah7jy3yzgnyzhp56f3v9jd7wj9j"))

(define rust-thiserror-2.0.21
  (crate-source "thiserror" "2.0.21"
                "17hq1lh5dyr3bkc7zzjrbrp4qgkvhc48kgq1n5fdxkindaw2rr89"))

(define rust-thiserror-impl-2.0.21
  (crate-source "thiserror-impl" "2.0.21"
                "0945n8agp7kg6n6b35yyjb4g5xv2q22vrw15h6jj1nw76a99flgy"))

(define rust-thread-local-1.1.10
  (crate-source "thread_local" "1.1.10"
                "0w20g2pfdcp8pz3gds0bzksv6mxk802szca8qlr3701jdm69rn8s"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.23
  (crate-source "tracing-subscriber" "0.3.23"
                "06fkr0qhggvrs861d7f74pn3i3a10h5jsp4n70jj9ys5b675fzyb"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-unicode-ident-1.0.27
  (crate-source "unicode-ident" "1.0.27"
                "0qv54hz0sxmspjyr162z3435pnyylzjjhx3j4k1s359pqgb59ix2"))

(define rust-unty-0.0.4
  (crate-source "unty" "0.0.4"
                "1blhyv01qiv5sb72sal3xa1l8nzck3answawxkkiw3fd2x1phjbd"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.26.1
  (crate-source "uuid" "1.26.1"
                "1kl5nb7r3gpmkc43d6nbayvzqhcp2grczk6c7bxv80b6x70xmxif"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-virtue-0.0.18
  (crate-source "virtue" "0.0.18"
                "1cgp79pzzs117kjlc3jnnkixbyaqri12j40mx2an41qhrymv27h5"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-wasip2-1.0.4+wasi-0.2.12
  (crate-source "wasip2" "1.0.4+wasi-0.2.12"
                "11wl7lqwq4pbmlmzr6n7bwz0hzy1z6sxc4554bkmrr86w4vznzmn"))

(define rust-wasm-bindgen-0.2.130
  (crate-source "wasm-bindgen" "0.2.130"
                "071xi9mqha1vdrxsjgndjgcf0azkl8g5yahkn2qbiiynvla1q650"))

(define rust-wasm-bindgen-macro-0.2.130
  (crate-source "wasm-bindgen-macro" "0.2.130"
                "1ncfybmdiczbsjz8a257a13k8n7dnl15khvgjrkbzgcslc11m4vf"))

(define rust-wasm-bindgen-macro-support-0.2.130
  (crate-source "wasm-bindgen-macro-support" "0.2.130"
                "0fm58fh6gkhlmi15rhzvcgz35x5s6kkvi9ik3s1vsmg4h11bb91c"))

(define rust-wasm-bindgen-shared-0.2.130
  (crate-source "wasm-bindgen-shared" "0.2.130"
                "17iq85zyk1z7hq533c0apj5ix0y0ks35fbszxv31cl3jk0vrq7rv"))

(define rust-web-sys-0.3.107
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.107"
                "125b0y934gyfwjkd60wnqjf2irmx250dar6vwinhzv7mdxcpklfp"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-sys-0.61.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-wit-bindgen-0.57.1
  (crate-source "wit-bindgen" "0.57.1"
                "0vjk2jb593ri9k1aq4iqs2si9mrw5q46wxnn78im7hm7hx799gqy"))

(define rust-xattr-1.7.0
  (crate-source "xattr" "1.7.0"
                "025fcy35jwf5bgm7305wj3jpa99wfk591gf83nblnl6mjamnygcn"))

(define rust-zerocopy-0.8.63
  (crate-source "zerocopy" "0.8.63"
                "1n1fm40mq4adw83hvn56j6rhnph0lldccx11jq01l6863f7izzp5"))

(define rust-zerocopy-derive-0.8.63
  (crate-source "zerocopy-derive" "0.8.63"
                "0zqc4g3yf378dkbsr68h1jjx3n0d4k4zvnxf83bv8gr9hfnd6fl6"))

(define rust-zlib-rs-0.6.8
  (crate-source "zlib-rs" "0.6.8"
                "04j158293bx73kv5pj1i89ai411q7fxc9zwk3wkpqgb9gj7fas5j"))

(define rust-zmij-1.0.23
  (crate-source "zmij" "1.0.23"
                "06zwri21nnrl34rwinmvbciap8yk1mrl8qfg9pff7lgspc56sri9"))

(define ssss-separator 'end-of-crates)

;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (pure-magic-rs =>
                                    (list rust-adler2-2.0.1
                                     rust-aho-corasick-1.1.5
                                     rust-android-system-properties-0.1.6
                                     rust-anes-0.1.6
                                     rust-anstream-1.0.0
                                     rust-anstyle-1.0.14
                                     rust-anstyle-parse-1.0.0
                                     rust-anstyle-query-1.1.5
                                     rust-anstyle-wincon-3.0.11
                                     rust-anyhow-1.0.104
                                     rust-ar-archive-writer-0.5.3
                                     rust-arrayvec-0.7.8
                                     rust-autocfg-1.5.1
                                     rust-bincode-2.0.1
                                     rust-bincode-derive-2.0.1
                                     rust-bitflags-2.13.2
                                     rust-blake3-1.8.7
                                     rust-bumpalo-3.20.3
                                     rust-cast-0.3.0
                                     rust-cc-1.11.0
                                     rust-cfg-if-1.0.5
                                     rust-chrono-0.4.45
                                     rust-ciborium-0.2.2
                                     rust-ciborium-io-0.2.2
                                     rust-ciborium-ll-0.2.2
                                     rust-clap-4.6.7
                                     rust-clap-builder-4.6.7
                                     rust-clap-derive-4.6.7
                                     rust-clap-lex-1.1.1
                                     rust-colorchoice-1.0.5
                                     rust-constant-time-eq-0.4.2
                                     rust-core-foundation-sys-0.8.7
                                     rust-cpufeatures-0.3.1
                                     rust-crc32fast-1.5.4
                                     rust-criterion-0.7.0
                                     rust-criterion-plot-0.6.0
                                     rust-crossbeam-deque-0.8.8
                                     rust-crossbeam-epoch-0.9.21
                                     rust-crossbeam-utils-0.8.23
                                     rust-crunchy-0.2.4
                                     rust-csv-1.4.0
                                     rust-csv-core-0.1.13
                                     rust-dyf-0.1.4
                                     rust-either-1.19.0
                                     rust-errno-0.3.14
                                     rust-fastrand-2.5.0
                                     rust-filetime-0.2.29
                                     rust-find-msvc-tools-0.1.15
                                     rust-flagset-0.4.7
                                     rust-flate2-1.1.10
                                     rust-fs-walk-0.2.2
                                     rust-futures-core-0.3.34
                                     rust-futures-task-0.3.34
                                     rust-futures-util-0.3.34
                                     rust-getrandom-0.3.4
                                     rust-getrandom-0.4.3
                                     rust-half-2.7.1
                                     rust-heck-0.5.0
                                     rust-iana-time-zone-0.1.65
                                     rust-iana-time-zone-haiku-0.1.2
                                     rust-is-terminal-polyfill-1.70.2
                                     rust-itertools-0.13.0
                                     rust-itoa-1.0.18
                                     rust-js-sys-0.3.107
                                     rust-lazy-static-1.5.1
                                     rust-libc-0.2.191
                                     rust-linux-raw-sys-0.12.1
                                     rust-log-0.4.35
                                     rust-matchers-0.2.0
                                     rust-memchr-2.8.3
                                     rust-memmap2-0.9.12
                                     rust-miniz-oxide-0.9.1
                                     rust-nu-ansi-term-0.50.3
                                     rust-num-traits-0.2.19
                                     rust-object-0.39.1
                                     rust-once-cell-1.21.4
                                     rust-once-cell-polyfill-1.70.2
                                     rust-oorandom-11.1.5
                                     rust-pest-2.9.5
                                     rust-pest-derive-2.9.5
                                     rust-pest-generator-2.9.5
                                     rust-pest-meta-2.9.5
                                     rust-pest-stack-guard-1.0.1
                                     rust-pin-project-lite-0.2.17
                                     rust-plotters-0.3.7
                                     rust-plotters-backend-0.3.7
                                     rust-plotters-svg-0.3.7
                                     rust-portable-atomic-1.15.0
                                     rust-ppv-lite86-0.2.21
                                     rust-proc-macro2-1.0.107
                                     rust-psm-0.1.32
                                     rust-pyo3-0.29.3
                                     rust-pyo3-build-config-0.29.3
                                     rust-pyo3-ffi-0.29.3
                                     rust-pyo3-macros-0.29.3
                                     rust-pyo3-macros-backend-0.29.3
                                     rust-quote-1.0.47
                                     rust-r-efi-5.3.0
                                     rust-r-efi-6.0.0
                                     rust-rand-0.9.5
                                     rust-rand-chacha-0.9.0
                                     rust-rand-core-0.9.5
                                     rust-rayon-1.12.0
                                     rust-rayon-core-1.13.0
                                     rust-regex-1.13.1
                                     rust-regex-automata-0.4.18
                                     rust-regex-syntax-0.8.11
                                     rust-rustix-1.1.6
                                     rust-rustversion-1.0.23
                                     rust-ryu-1.0.23
                                     rust-same-file-1.0.6
                                     rust-serde-1.0.229
                                     rust-serde-core-1.0.229
                                     rust-serde-derive-1.0.229
                                     rust-serde-json-1.0.154
                                     rust-sharded-slab-0.1.7
                                     rust-shlex-2.0.1
                                     rust-simd-adler32-0.3.10
                                     rust-slab-0.4.12
                                     rust-smallvec-1.16.4
                                     rust-stacker-0.1.25
                                     rust-strsim-0.11.1
                                     rust-syn-2.0.119
                                     rust-syn-3.0.9
                                     rust-tar-0.4.46
                                     rust-target-lexicon-0.13.5
                                     rust-tempfile-3.27.0
                                     rust-thiserror-2.0.21
                                     rust-thiserror-impl-2.0.21
                                     rust-thread-local-1.1.10
                                     rust-tinytemplate-1.2.1
                                     rust-tracing-0.1.44
                                     rust-tracing-attributes-0.1.31
                                     rust-tracing-core-0.1.36
                                     rust-tracing-log-0.2.0
                                     rust-tracing-subscriber-0.3.23
                                     rust-ucd-trie-0.1.7
                                     rust-unicode-ident-1.0.27
                                     rust-unty-0.0.4
                                     rust-utf8parse-0.2.2
                                     rust-uuid-1.26.1
                                     rust-valuable-0.1.1
                                     rust-virtue-0.0.18
                                     rust-walkdir-2.5.0
                                     rust-wasip2-1.0.4+wasi-0.2.12
                                     rust-wasm-bindgen-0.2.130
                                     rust-wasm-bindgen-macro-0.2.130
                                     rust-wasm-bindgen-macro-support-0.2.130
                                     rust-wasm-bindgen-shared-0.2.130
                                     rust-web-sys-0.3.107
                                     rust-winapi-util-0.1.11
                                     rust-windows-core-0.62.2
                                     rust-windows-implement-0.60.2
                                     rust-windows-interface-0.59.3
                                     rust-windows-link-0.2.1
                                     rust-windows-result-0.4.1
                                     rust-windows-strings-0.5.1
                                     rust-windows-sys-0.61.2
                                     rust-wit-bindgen-0.57.1
                                     rust-xattr-1.7.0
                                     rust-zerocopy-0.8.63
                                     rust-zerocopy-derive-0.8.63
                                     rust-zlib-rs-0.6.8
                                     rust-zmij-1.0.23)))
