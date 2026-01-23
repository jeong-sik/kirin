(** i18n Module Tests (Phase 15) *)

open Kirin

let basic_tests = [
  "create", `Quick, (fun () ->
    let i18n = I18n.create () in
    Alcotest.(check string) "default locale" "en" (I18n.current_locale i18n)
  );

  "create with default locale", `Quick, (fun () ->
    let i18n = I18n.create ~default_locale:"ko" () in
    Alcotest.(check string) "default locale" "ko" (I18n.current_locale i18n)
  );

  "add translations", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [
           ("greeting", "Hello");
           ("farewell", "Goodbye");
         ] in
    Alcotest.(check bool) "has locale" true (I18n.has_locale "en" i18n)
  );

  "available locales", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")]
      |> I18n.add_translations "ko" [("greeting", "안녕하세요")] in
    let locales = I18n.available_locales i18n in
    Alcotest.(check int) "locale count" 2 (List.length locales)
  );
]

let translation_tests = [
  "simple translation", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")] in
    let msg = I18n.translate i18n ~locale:"en" "greeting" in
    Alcotest.(check string) "translation" "Hello" msg
  );

  "translation with interpolation", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello, {{name}}!")] in
    let msg = I18n.translate i18n ~locale:"en" ~args:[("name", "World")] "greeting" in
    Alcotest.(check string) "translation" "Hello, World!" msg
  );

  "multiple interpolations", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [
           ("message", "{{user}} sent {{count}} messages");
         ] in
    let msg = I18n.translate i18n ~locale:"en"
      ~args:[("user", "Alice"); ("count", "5")] "message" in
    Alcotest.(check string) "translation" "Alice sent 5 messages" msg
  );

  "missing key returns key", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")] in
    let msg = I18n.translate i18n ~locale:"en" "unknown.key" in
    Alcotest.(check string) "returns key" "unknown.key" msg
  );

  "fallback to default locale", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")]
      |> I18n.add_translations "ko" [] in
    let msg = I18n.translate i18n ~locale:"ko" "greeting" in
    Alcotest.(check string) "fallback" "Hello" msg
  );

  "korean translation", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "ko" [
           ("greeting", "안녕하세요, {{name}}님!");
         ] in
    let msg = I18n.translate i18n ~locale:"ko" ~args:[("name", "홍길동")] "greeting" in
    Alcotest.(check string) "korean" "안녕하세요, 홍길동님!" msg
  );
]

let plural_tests = [
  "english plural one", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_plural "en" ~key:"items" ~one:"{{count}} item" ~other:"{{count}} items" in
    let msg = I18n.translate i18n ~locale:"en" ~count:1 "items" in
    Alcotest.(check string) "one" "1 item" msg
  );

  "english plural other", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_plural "en" ~key:"items" ~one:"{{count}} item" ~other:"{{count}} items" in
    let msg = I18n.translate i18n ~locale:"en" ~count:5 "items" in
    Alcotest.(check string) "other" "5 items" msg
  );

  "english plural zero as other", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_plural "en" ~key:"items" ~one:"{{count}} item" ~other:"{{count}} items" in
    let msg = I18n.translate i18n ~locale:"en" ~count:0 "items" in
    Alcotest.(check string) "zero as other" "0 items" msg
  );

  "french plural zero/one same", `Quick, (fun () ->
    (* French treats 0 and 1 as singular *)
    let i18n = I18n.create ()
      |> I18n.add_plural "fr" ~key:"items"
           ~one:"{{count}} élément"
           ~other:"{{count}} éléments" in
    let msg0 = I18n.translate i18n ~locale:"fr" ~count:0 "items" in
    let msg1 = I18n.translate i18n ~locale:"fr" ~count:1 "items" in
    Alcotest.(check string) "zero" "0 élément" msg0;
    Alcotest.(check string) "one" "1 élément" msg1
  );

  "korean no plural", `Quick, (fun () ->
    (* Korean has no plural distinction *)
    let i18n = I18n.create ()
      |> I18n.add_plural "ko" ~key:"items"
           ~one:"{{count}}개"  (* Won't be used *)
           ~other:"{{count}}개" in
    let msg1 = I18n.translate i18n ~locale:"ko" ~count:1 "items" in
    let msg5 = I18n.translate i18n ~locale:"ko" ~count:5 "items" in
    Alcotest.(check string) "one" "1개" msg1;
    Alcotest.(check string) "five" "5개" msg5
  );

  "russian plural few", `Quick, (fun () ->
    (* Russian has complex plural rules *)
    let i18n = I18n.create ()
      |> I18n.add_plural "ru" ~key:"items"
           ~one:"{{count}} элемент"
           ~few:"{{count}} элемента"
           ~many:"{{count}} элементов"
           ~other:"{{count}} элементов" in
    let msg1 = I18n.translate i18n ~locale:"ru" ~count:1 "items" in
    let msg2 = I18n.translate i18n ~locale:"ru" ~count:2 "items" in
    let msg5 = I18n.translate i18n ~locale:"ru" ~count:5 "items" in
    Alcotest.(check string) "one" "1 элемент" msg1;
    Alcotest.(check string) "few" "2 элемента" msg2;
    Alcotest.(check string) "many" "5 элементов" msg5
  );

  "arabic plural zero", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_plural "ar" ~key:"items"
           ~zero:"لا عناصر"
           ~one:"عنصر واحد"
           ~other:"عناصر" in
    let msg = I18n.translate i18n ~locale:"ar" ~count:0 "items" in
    Alcotest.(check string) "zero" "لا عناصر" msg
  );
]

let accept_language_tests = [
  "parse simple", `Quick, (fun () ->
    let locales = I18n.parse_accept_language "en" in
    Alcotest.(check (list string)) "locales" ["en"] locales
  );

  "parse with region", `Quick, (fun () ->
    let locales = I18n.parse_accept_language "en-US" in
    Alcotest.(check (list string)) "locales" ["en-US"] locales
  );

  "parse multiple", `Quick, (fun () ->
    let locales = I18n.parse_accept_language "en-US,en;q=0.9,ko;q=0.8" in
    Alcotest.(check (list string)) "locales" ["en-US"; "en"; "ko"] locales
  );

  "parse with quality sort", `Quick, (fun () ->
    let locales = I18n.parse_accept_language "ko;q=0.5,en;q=0.9,ja;q=0.7" in
    Alcotest.(check (list string)) "sorted by quality" ["en"; "ja"; "ko"] locales
  );

  "detect locale", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")]
      |> I18n.add_translations "ko" [("greeting", "안녕하세요")] in
    let locale = I18n.detect_locale i18n "ko-KR,ko;q=0.9,en;q=0.8" in
    Alcotest.(check string) "detected" "ko" locale
  );

  "detect locale fallback", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")] in
    let locale = I18n.detect_locale i18n "ja,zh" in
    Alcotest.(check string) "fallback to default" "en" locale
  );
]

let translator_tests = [
  "translator for header", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello, {{name}}!")]
      |> I18n.add_translations "ko" [("greeting", "안녕하세요, {{name}}님!")] in
    let t = I18n.translator_for_header i18n (Some "ko") in
    let msg = t "greeting" ~args:[("name", "홍길동")] () in
    Alcotest.(check string) "korean translator" "안녕하세요, 홍길동님!" msg
  );

  "translator for none header", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")] in
    let t = I18n.translator_for_header i18n None in
    let msg = t "greeting" () in
    Alcotest.(check string) "default locale" "Hello" msg
  );

  "set locale from header", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")]
      |> I18n.add_translations "ko" [("greeting", "안녕하세요")] in
    let locale = I18n.set_locale_from_header i18n (Some "ko-KR,en") in
    Alcotest.(check string) "set locale" "ko" locale;
    Alcotest.(check string) "current locale" "ko" (I18n.current_locale i18n)
  );
]

let locale_management_tests = [
  "set locale", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" []
      |> I18n.add_translations "ko" [] in
    I18n.set_locale "ko" i18n;
    Alcotest.(check string) "current" "ko" (I18n.current_locale i18n)
  );

  "set fallback", `Quick, (fun () ->
    let i18n = I18n.create ()
      |> I18n.add_translations "en" [("greeting", "Hello")]
      |> I18n.add_translations "en-US" []
      |> I18n.set_fallback ~locale:"en-US" ~fallback:"en" in
    let msg = I18n.translate i18n ~locale:"en-US" "greeting" in
    Alcotest.(check string) "fallback used" "Hello" msg
  );
]

let formatting_tests = [
  "format number en", `Quick, (fun () ->
    let formatted = I18n.format_number ~locale:"en" 1234567.89 in
    Alcotest.(check string) "english format" "1,234,567.89" formatted
  );

  "format number de", `Quick, (fun () ->
    let formatted = I18n.format_number ~locale:"de" 1234567.89 in
    Alcotest.(check string) "german format" "1.234.567,89" formatted
  );

  "format currency usd", `Quick, (fun () ->
    let formatted = I18n.format_currency ~locale:"en" ~currency:"USD" 99.99 in
    Alcotest.(check string) "usd" "$99.99" formatted
  );

  "format currency eur", `Quick, (fun () ->
    let formatted = I18n.format_currency ~locale:"de" ~currency:"EUR" 99.99 in
    Alcotest.(check string) "eur" "99,99 €" formatted
  );

  "format currency krw", `Quick, (fun () ->
    let formatted = I18n.format_currency ~locale:"ko" ~currency:"KRW" 10000. in
    Alcotest.(check string) "krw" "₩10,000" formatted
  );

  "format date en", `Quick, (fun () ->
    let formatted = I18n.format_date ~locale:"en" ~year:2024 ~month:12 ~day:25 () in
    Alcotest.(check string) "en date" "12/25/2024" formatted
  );

  "format date ko", `Quick, (fun () ->
    let formatted = I18n.format_date ~locale:"ko" ~year:2024 ~month:12 ~day:25 () in
    Alcotest.(check string) "ko date" "2024년 12월 25일" formatted
  );

  "format date de", `Quick, (fun () ->
    let formatted = I18n.format_date ~locale:"de" ~year:2024 ~month:12 ~day:25 () in
    Alcotest.(check string) "de date" "25.12.2024" formatted
  );
]

let () =
  Alcotest.run "I18n" [
    ("Basic", basic_tests);
    ("Translation", translation_tests);
    ("Plural", plural_tests);
    ("Accept-Language", accept_language_tests);
    ("Translator", translator_tests);
    ("Locale Management", locale_management_tests);
    ("Formatting", formatting_tests);
  ]
