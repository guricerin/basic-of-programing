namespace MetroNetwork

open MetroNetwork.Ekimei
open MetroNetwork.Ekikan

module Metro =

    let globalEkimeiList = [
        {kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"};
        {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"};
        {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"};
        {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"};
        {kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; shozoku="千代田線"};
        {kanji="赤坂"; kana="あかさか"; romaji="akasaka"; shozoku="千代田線"};
        {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidoumae"; shozoku="千代田線"};
        {kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="千代田線"};
        {kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="千代田線"};
        {kanji="二重橋前"; kana="にじゅうばしまえ"; romaji="nijuubasimae"; shozoku="千代田線"};
        {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="千代田線"};
        {kanji="新御茶ノ水"; kana="しんおちゃのみず"; romaji="shin-ochanomizu"; shozoku="千代田線"};
        {kanji="湯島"; kana="ゆしま"; romaji="yushima"; shozoku="千代田線"};
        {kanji="根津"; kana="ねづ"; romaji="nedu"; shozoku="千代田線"};
        {kanji="千駄木"; kana="せんだぎ"; romaji="sendagi"; shozoku="千代田線"};
        {kanji="西日暮里"; kana="にしにっぽり"; romaji="nishinippori"; shozoku="千代田線"};
        {kanji="町屋"; kana="まちや"; romaji="machiya"; shozoku="千代田線"};
        {kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenjyu"; shozoku="千代田線"};
        {kanji="綾瀬"; kana="あやせ"; romaji="ayase"; shozoku="千代田線"};
        {kanji="北綾瀬"; kana="きたあやせ"; romaji="kitaayase"; shozoku="千代田線"};
        {kanji="浅草"; kana="あさくさ"; romaji="asakusa"; shozoku="銀座線"};
        {kanji="田原町"; kana="たわらまち"; romaji="tawaramachi"; shozoku="銀座線"};
        {kanji="稲荷町"; kana="いなりちょう"; romaji="inaricho"; shozoku="銀座線"};
        {kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="銀座線"};
        {kanji="上野広小路"; kana="うえのひろこうじ"; romaji="uenohirokoji"; shozoku="銀座線"};
        {kanji="末広町"; kana="すえひろちょう"; romaji="suehirocho"; shozoku="銀座線"};
        {kanji="神田"; kana="かんだ"; romaji="kanda"; shozoku="銀座線"};
        {kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="銀座線"};
        {kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="銀座線"};
        {kanji="京橋"; kana="きょうばし"; romaji="kyobashi"; shozoku="銀座線"};
        {kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="銀座線"};
        {kanji="新橋"; kana="しんばし"; romaji="shinbashi"; shozoku="銀座線"};
        {kanji="虎ノ門"; kana="とらのもん"; romaji="toranomon"; shozoku="銀座線"};
        {kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesannou"; shozoku="銀座線"};
        {kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="銀座線"};
        {kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyamaicchome"; shozoku="銀座線"};
        {kanji="外苑前"; kana="がいえんまえ"; romaji="gaienmae"; shozoku="銀座線"};
        {kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; shozoku="銀座線"};
        {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="銀座線"};
        {kanji="渋谷"; kana="しぶや"; romaji="shibuya"; shozoku="半蔵門線"};
        {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="半蔵門線"};
        {kanji="青山一丁目"; kana="あおやまいっちょうめ"; romaji="aoyama-itchome"; shozoku="半蔵門線"};
        {kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="半蔵門線"};
        {kanji="半蔵門"; kana="はんぞうもん"; romaji="hanzomon"; shozoku="半蔵門線"};
        {kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="半蔵門線"};
        {kanji="神保町"; kana="じんぼうちょう"; romaji="jinbocho"; shozoku="半蔵門線"};
        {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="半蔵門線"};
        {kanji="三越前"; kana="みつこしまえ"; romaji="mitsukoshimae"; shozoku="半蔵門線"};
        {kanji="水天宮前"; kana="すいてんぐうまえ"; romaji="suitengumae"; shozoku="半蔵門線"};
        {kanji="清澄白河"; kana="きよすみしらかわ"; romaji="kiyosumi-shirakawa"; shozoku="半蔵門線"};
        {kanji="住吉"; kana="すみよし"; romaji="sumiyoshi"; shozoku="半蔵門線"};
        {kanji="錦糸町"; kana="きんしちょう"; romaji="kinshicho"; shozoku="半蔵門線"};
        {kanji="押上"; kana="おしあげ"; romaji="oshiage"; shozoku="半蔵門線"};
        {kanji="中目黒"; kana="なかめぐろ"; romaji="nakameguro"; shozoku="日比谷線"};
        {kanji="恵比寿"; kana="えびす"; romaji="ebisu"; shozoku="日比谷線"};
        {kanji="広尾"; kana="ひろお"; romaji="hiro"; shozoku="日比谷線"};
        {kanji="六本木"; kana="ろっぽんぎ"; romaji="roppongi"; shozoku="日比谷線"};
        {kanji="神谷町"; kana="かみやちょう"; romaji="kamiyacho"; shozoku="日比谷線"};
        {kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="日比谷線"};
        {kanji="日比谷"; kana="ひびや"; romaji="hibiya"; shozoku="日比谷線"};
        {kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="日比谷線"};
        {kanji="東銀座"; kana="ひがしぎんざ"; romaji="higashiginza"; shozoku="日比谷線"};
        {kanji="築地"; kana="つきじ"; romaji="tsukiji"; shozoku="日比谷線"};
        {kanji="八丁堀"; kana="はっちょうぼり"; romaji="hacchobori"; shozoku="日比谷線"};
        {kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="日比谷線"};
        {kanji="人形町"; kana="にんぎょうちょう"; romaji="ningyomachi"; shozoku="日比谷線"};
        {kanji="小伝馬町"; kana="こでんまちょう"; romaji="kodemmacho"; shozoku="日比谷線"};
        {kanji="秋葉原"; kana="あきはばら"; romaji="akihabara"; shozoku="日比谷線"};
        {kanji="仲御徒町"; kana="なかおかちまち"; romaji="nakaokachimachi"; shozoku="日比谷線"};
        {kanji="上野"; kana="うえの"; romaji="ueno"; shozoku="日比谷線"};
        {kanji="入谷"; kana="いりや"; romaji="iriya"; shozoku="日比谷線"};
        {kanji="三ノ輪"; kana="みのわ"; romaji="minowa"; shozoku="日比谷線"};
        {kanji="南千住"; kana="みなみせんじゅ"; romaji="minamisenju"; shozoku="日比谷線"};
        {kanji="北千住"; kana="きたせんじゅ"; romaji="kitasenju"; shozoku="日比谷線"};
        {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"};
        {kanji="新大塚"; kana="しんおおつか"; romaji="shinotsuka"; shozoku="丸ノ内線"};
        {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"};
        {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="丸ノ内線"};
        {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; romaji="hongosanchome"; shozoku="丸ノ内線"};
        {kanji="御茶ノ水"; kana="おちゃのみず"; romaji="ochanomizu"; shozoku="丸ノ内線"};
        {kanji="淡路町"; kana="あわじちょう"; romaji="awajicho"; shozoku="丸ノ内線"};
        {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="丸ノ内線"};
        {kanji="東京"; kana="とうきょう"; romaji="tokyo"; shozoku="丸ノ内線"};
        {kanji="銀座"; kana="ぎんざ"; romaji="ginza"; shozoku="丸ノ内線"};
        {kanji="霞ヶ関"; kana="かすみがせき"; romaji="kasumigaseki"; shozoku="丸ノ内線"};
        {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; romaji="kokkaigijidomae"; shozoku="丸ノ内線"};
        {kanji="赤坂見附"; kana="あかさかみつけ"; romaji="akasakamitsuke"; shozoku="丸ノ内線"};
        {kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="丸ノ内線"};
        {kanji="四谷三丁目"; kana="よつやさんちょうめ"; romaji="yotsuyasanchome"; shozoku="丸ノ内線"};
        {kanji="新宿御苑前"; kana="しんじゅくぎょえんまえ"; romaji="shinjuku-gyoemmae"; shozoku="丸ノ内線"};
        {kanji="新宿三丁目"; kana="しんじゅくさんちょうめ"; romaji="shinjuku-sanchome"; shozoku="丸ノ内線"};
        {kanji="新宿"; kana="しんじゅく"; romaji="shinjuku"; shozoku="丸ノ内線"};
        {kanji="西新宿"; kana="にししんじゅく"; romaji="nishi-shinjuku"; shozoku="丸ノ内線"};
        {kanji="中野坂上"; kana="なかのさかうえ"; romaji="nakano-sakaue"; shozoku="丸ノ内線"};
        {kanji="新中野"; kana="しんなかの"; romaji="shin-nakano"; shozoku="丸ノ内線"};
        {kanji="東高円寺"; kana="ひがしこうえんじ"; romaji="higashi-koenji"; shozoku="丸ノ内線"};
        {kanji="新高円寺"; kana="しんこうえんじ"; romaji="shin-koenji"; shozoku="丸ノ内線"};
        {kanji="南阿佐ヶ谷"; kana="みなみあさがや"; romaji="minami-asagaya"; shozoku="丸ノ内線"};
        {kanji="荻窪"; kana="おぎくぼ"; romaji="ogikubo"; shozoku="丸ノ内線"};
        {kanji="中野新橋"; kana="なかのしんばし"; romaji="nakano-shimbashi"; shozoku="丸ノ内線"};
        {kanji="中野富士見町"; kana="なかのふじみちょう"; romaji="nakano-fujimicho"; shozoku="丸ノ内線"};
        {kanji="方南町"; kana="ほうなんちょう"; romaji="honancho"; shozoku="丸ノ内線"};
        {kanji="四ツ谷"; kana="よつや"; romaji="yotsuya"; shozoku="南北線"};
        {kanji="永田町"; kana="ながたちょう"; romaji="nagatacho"; shozoku="南北線"};
        {kanji="溜池山王"; kana="ためいけさんのう"; romaji="tameikesanno"; shozoku="南北線"};
        {kanji="六本木一丁目"; kana="ろっぽんぎいっちょうめ"; romaji="roppongiitchome"; shozoku="南北線"};
        {kanji="麻布十番"; kana="あざぶじゅうばん"; romaji="azabujuban"; shozoku="南北線"};
        {kanji="白金高輪"; kana="しろかねたかなわ"; romaji="shirokanetakanawa"; shozoku="南北線"};
        {kanji="白金台"; kana="しろかねだい"; romaji="shirokanedai"; shozoku="南北線"};
        {kanji="目黒"; kana="めぐろ"; romaji="meguro"; shozoku="南北線"};
        {kanji="市ヶ谷"; kana="いちがや"; romaji="ichigaya"; shozoku="南北線"};
        {kanji="飯田橋"; kana="いいだばし"; romaji="idabashi"; shozoku="南北線"};
        {kanji="後楽園"; kana="こうらくえん"; romaji="korakuen"; shozoku="南北線"};
        {kanji="東大前"; kana="とうだいまえ"; romaji="todaimae"; shozoku="南北線"};
        {kanji="本駒込"; kana="ほんこまごめ"; romaji="honkomagome"; shozoku="南北線"};
        {kanji="駒込"; kana="こまごめ"; romaji="komagome"; shozoku="南北線"};
        {kanji="西ヶ原"; kana="にしがはら"; romaji="nishigahara"; shozoku="南北線"};
        {kanji="王子"; kana="おうじ"; romaji="oji"; shozoku="南北線"};
        {kanji="王子神谷"; kana="おうじかみや"; romaji="ojikamiya"; shozoku="南北線"};
        {kanji="志茂"; kana="しも"; romaji="shimo"; shozoku="南北線"};
        {kanji="赤羽岩淵"; kana="あかばねいわぶち"; romaji="akabaneiwabuchi"; shozoku="南北線"};
        {kanji="西船橋"; kana="にしふなばし"; romaji="nishi-funabashi"; shozoku="東西線"};
        {kanji="原木中山"; kana="ばらきなかやま"; romaji="baraki-nakayama"; shozoku="東西線"};
        {kanji="妙典"; kana="みょうでん"; romaji="myoden"; shozoku="東西線"};
        {kanji="行徳"; kana="ぎょうとく"; romaji="gyotoku"; shozoku="東西線"};
        {kanji="南行徳"; kana="みなみぎょうとく"; romaji="minami-gyotoku"; shozoku="東西線"};
        {kanji="浦安"; kana="うらやす"; romaji="urayasu"; shozoku="東西線"};
        {kanji="葛西"; kana="かさい"; romaji="kasai"; shozoku="東西線"};
        {kanji="西葛西"; kana="にしかさい"; romaji="nishi-kasai"; shozoku="東西線"};
        {kanji="南砂町"; kana="みなみすなまち"; romaji="minami-sunamachi"; shozoku="東西線"};
        {kanji="東陽町"; kana="とうようちょう"; romaji="touyoucho"; shozoku="東西線"};
        {kanji="木場"; kana="きば"; romaji="kiba"; shozoku="東西線"};
        {kanji="門前仲町"; kana="もんぜんなかちょう"; romaji="monzen-nakacho"; shozoku="東西線"};
        {kanji="茅場町"; kana="かやばちょう"; romaji="kayabacho"; shozoku="東西線"};
        {kanji="日本橋"; kana="にほんばし"; romaji="nihonbashi"; shozoku="東西線"};
        {kanji="大手町"; kana="おおてまち"; romaji="otemachi"; shozoku="東西線"};
        {kanji="竹橋"; kana="たけばし"; romaji="takebashi"; shozoku="東西線"};
        {kanji="九段下"; kana="くだんした"; romaji="kudanshita"; shozoku="東西線"};
        {kanji="飯田橋"; kana="いいだばし"; romaji="iidabashi"; shozoku="東西線"};
        {kanji="神楽坂"; kana="かぐらざか"; romaji="kagurazaka"; shozoku="東西線"};
        {kanji="早稲田"; kana="わせだ"; romaji="waseda"; shozoku="東西線"};
        {kanji="高田馬場"; kana="たかだのばば"; romaji="takadanobaba"; shozoku="東西線"};
        {kanji="落合"; kana="おちあい"; romaji="ochiai"; shozoku="東西線"};
        {kanji="中野"; kana="なかの"; romaji="nakano"; shozoku="東西線"};
        {romaji="shinkiba"; kana="しんきば"; kanji="新木場"; shozoku="有楽町線"};
        {romaji="tatsumi"; kana="たつみ"; kanji="辰巳"; shozoku="有楽町線"};
        {romaji="toyosu"; kana="とよす"; kanji="豊洲"; shozoku="有楽町線"};
        {romaji="tsukishima"; kana="つきしま"; kanji="月島"; shozoku="有楽町線"};
        {romaji="shintomityou"; kana="しんとみちょう"; kanji="新富町"; shozoku="有楽町線"};
        {romaji="ginzaittyoume"; kana="ぎんざいっちょうめ"; kanji="銀座一丁目"; shozoku="有楽町線"};
        {romaji="yuurakutyou"; kana="ゆうらくちょう"; kanji="有楽町"; shozoku="有楽町線"};
        {romaji="sakuradamon"; kana="さくらだもん"; kanji="桜田門"; shozoku="有楽町線"};
        {romaji="nagatacho"; kana="ながたちょう"; kanji="永田町"; shozoku="有楽町線"};
        {romaji="koujimachi"; kana="こうじまち"; kanji="麹町"; shozoku="有楽町線"};
        {romaji="ichigaya"; kana="いちがや"; kanji="市ヶ谷"; shozoku="有楽町線"};
        {romaji="iidabashi"; kana="いいだばし"; kanji="飯田橋"; shozoku="有楽町線"};
        {kanji="江戸川橋"; kana="えどがわばし"; romaji="edogawabasi"; shozoku="有楽町線"};
        {kanji="護国寺"; kana="ごこくじ"; romaji="gokokuji"; shozoku="有楽町線"};
        {kanji="東池袋"; kana="ひがしいけぶくろ"; romaji="higasiikebukuro"; shozoku="有楽町線"};
        {kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"};
        {kanji="要町"; kana="かなめちょう"; romaji="kanametyou"; shozoku="有楽町線"};
        {kanji="千川"; kana="せんかわ"; romaji="senkawa"; shozoku="有楽町線"};
        {kanji="小竹向原"; kana="こたけむかいはら"; romaji="kotakemukaihara"; shozoku="有楽町線"};
        {kanji="氷川台"; kana="ひかわだい"; romaji="hikawadai"; shozoku="有楽町線"};
        {kanji="平和台"; kana="へいわだい"; romaji="heiwadai"; shozoku="有楽町線"};
        {kanji="営団赤塚"; kana="えいだんあかつか"; romaji="eidanakakuka"; shozoku="有楽町線"};
        {kanji="営団成増"; kana="えいだんなります"; romaji="eidannarimasu"; shozoku="有楽町線"};
        {kanji="和光市"; kana="わこうし"; romaji="wakousi"; shozoku="有楽町線"};
    ]

    let globalEkikanList = [
        {kiten="代々木上原"; shuten="代々木公園"; keiyu="千代田線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="代々木公園"; shuten="明治神宮前"; keiyu="千代田線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="明治神宮前"; shuten="表参道"; keiyu="千代田線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="表参道"; shuten="乃木坂"; keiyu="千代田線"; kyori=1.4<km>; jikan=3<minute>};
        {kiten="乃木坂"; shuten="赤坂"; keiyu="千代田線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="赤坂"; shuten="国会議事堂前"; keiyu="千代田線"; kyori=0.8<km>; jikan=1<minute>};
        {kiten="国会議事堂前"; shuten="霞ヶ関"; keiyu="千代田線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="霞ヶ関"; shuten="日比谷"; keiyu="千代田線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="日比谷"; shuten="二重橋前"; keiyu="千代田線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="二重橋前"; shuten="大手町"; keiyu="千代田線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="大手町"; shuten="新御茶ノ水"; keiyu="千代田線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="新御茶ノ水"; shuten="湯島"; keiyu="千代田線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="湯島"; shuten="根津"; keiyu="千代田線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="根津"; shuten="千駄木"; keiyu="千代田線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="千駄木"; shuten="西日暮里"; keiyu="千代田線"; kyori=0.9<km>; jikan=1<minute>};
        {kiten="西日暮里"; shuten="町屋"; keiyu="千代田線"; kyori=1.7<km>; jikan=2<minute>};
        {kiten="町屋"; shuten="北千住"; keiyu="千代田線"; kyori=2.6<km>; jikan=3<minute>};
        {kiten="北千住"; shuten="綾瀬"; keiyu="千代田線"; kyori=2.5<km>; jikan=3<minute>};
        {kiten="綾瀬"; shuten="北綾瀬"; keiyu="千代田線"; kyori=2.1<km>; jikan=4<minute>};
        {kiten="浅草"; shuten="田原町"; keiyu="銀座線"; kyori=0.8<km>; jikan=2<minute>};
        {kiten="田原町"; shuten="稲荷町"; keiyu="銀座線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="稲荷町"; shuten="上野"; keiyu="銀座線"; kyori=0.7<km>; jikan=2<minute>};
        {kiten="上野"; shuten="上野広小路"; keiyu="銀座線"; kyori=0.5<km>; jikan=2<minute>};
        {kiten="上野広小路"; shuten="末広町"; keiyu="銀座線"; kyori=0.6<km>; jikan=1<minute>};
        {kiten="末広町"; shuten="神田"; keiyu="銀座線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="神田"; shuten="三越前"; keiyu="銀座線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="三越前"; shuten="日本橋"; keiyu="銀座線"; kyori=0.6<km>; jikan=2<minute>};
        {kiten="日本橋"; shuten="京橋"; keiyu="銀座線"; kyori=0.7<km>; jikan=2<minute>};
        {kiten="京橋"; shuten="銀座"; keiyu="銀座線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="銀座"; shuten="新橋"; keiyu="銀座線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="新橋"; shuten="虎ノ門"; keiyu="銀座線"; kyori=0.8<km>; jikan=2<minute>};
        {kiten="虎ノ門"; shuten="溜池山王"; keiyu="銀座線"; kyori=0.6<km>; jikan=2<minute>};
        {kiten="溜池山王"; shuten="赤坂見附"; keiyu="銀座線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="赤坂見附"; shuten="青山一丁目"; keiyu="銀座線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="青山一丁目"; shuten="外苑前"; keiyu="銀座線"; kyori=0.7<km>; jikan=2<minute>};
        {kiten="外苑前"; shuten="表参道"; keiyu="銀座線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="表参道"; shuten="渋谷"; keiyu="銀座線"; kyori=1.3<km>; jikan=1<minute>};
        {kiten="渋谷"; shuten="表参道"; keiyu="半蔵門線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="表参道"; shuten="青山一丁目"; keiyu="半蔵門線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="青山一丁目"; shuten="永田町"; keiyu="半蔵門線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="永田町"; shuten="半蔵門"; keiyu="半蔵門線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="半蔵門"; shuten="九段下"; keiyu="半蔵門線"; kyori=1.6<km>; jikan=2<minute>};
        {kiten="九段下"; shuten="神保町"; keiyu="半蔵門線"; kyori=0.4<km>; jikan=1<minute>};
        {kiten="神保町"; shuten="大手町"; keiyu="半蔵門線"; kyori=1.7<km>; jikan=3<minute>};
        {kiten="大手町"; shuten="三越前"; keiyu="半蔵門線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="三越前"; shuten="水天宮前"; keiyu="半蔵門線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="水天宮前"; shuten="清澄白河"; keiyu="半蔵門線"; kyori=1.7<km>; jikan=3<minute>};
        {kiten="清澄白河"; shuten="住吉"; keiyu="半蔵門線"; kyori=1.9<km>; jikan=3<minute>};
        {kiten="住吉"; shuten="錦糸町"; keiyu="半蔵門線"; kyori=1.<km>; jikan=2<minute>};
        {kiten="錦糸町"; shuten="押上"; keiyu="半蔵門線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="中目黒"; shuten="恵比寿"; keiyu="日比谷線"; kyori=1.<km>; jikan=2<minute>};
        {kiten="恵比寿"; shuten="広尾"; keiyu="日比谷線"; kyori=1.5<km>; jikan=3<minute>};
        {kiten="広尾"; shuten="六本木"; keiyu="日比谷線"; kyori=1.7<km>; jikan=3<minute>};
        {kiten="六本木"; shuten="神谷町"; keiyu="日比谷線"; kyori=1.5<km>; jikan=3<minute>};
        {kiten="神谷町"; shuten="霞ヶ関"; keiyu="日比谷線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="霞ヶ関"; shuten="日比谷"; keiyu="日比谷線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="日比谷"; shuten="銀座"; keiyu="日比谷線"; kyori=0.4<km>; jikan=1<minute>};
        {kiten="銀座"; shuten="東銀座"; keiyu="日比谷線"; kyori=0.4<km>; jikan=1<minute>};
        {kiten="東銀座"; shuten="築地"; keiyu="日比谷線"; kyori=0.6<km>; jikan=2<minute>};
        {kiten="築地"; shuten="八丁堀"; keiyu="日比谷線"; kyori=1.<km>; jikan=2<minute>};
        {kiten="八丁堀"; shuten="茅場町"; keiyu="日比谷線"; kyori=0.5<km>; jikan=1<minute>};
        {kiten="茅場町"; shuten="人形町"; keiyu="日比谷線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="人形町"; shuten="小伝馬町"; keiyu="日比谷線"; kyori=0.6<km>; jikan=1<minute>};
        {kiten="小伝馬町"; shuten="秋葉原"; keiyu="日比谷線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="秋葉原"; shuten="仲御徒町"; keiyu="日比谷線"; kyori=1.<km>; jikan=1<minute>};
        {kiten="仲御徒町"; shuten="上野"; keiyu="日比谷線"; kyori=0.5<km>; jikan=1<minute>};
        {kiten="上野"; shuten="入谷"; keiyu="日比谷線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="入谷"; shuten="三ノ輪"; keiyu="日比谷線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="三ノ輪"; shuten="南千住"; keiyu="日比谷線"; kyori=0.8<km>; jikan=2<minute>};
        {kiten="南千住"; shuten="北千住"; keiyu="日比谷線"; kyori=1.8<km>; jikan=3<minute>};
        {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8<km>; jikan=3<minute>};
        {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8<km>; jikan=2<minute>};
        {kiten="後楽園"; shuten="本郷三丁目"; keiyu="丸ノ内線"; kyori=0.8<km>; jikan=1<minute>};
        {kiten="本郷三丁目"; shuten="御茶ノ水"; keiyu="丸ノ内線"; kyori=0.8<km>; jikan=1<minute>};
        {kiten="御茶ノ水"; shuten="淡路町"; keiyu="丸ノ内線"; kyori=0.8<km>; jikan=1<minute>};
        {kiten="淡路町"; shuten="大手町"; keiyu="丸ノ内線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="大手町"; shuten="東京"; keiyu="丸ノ内線"; kyori=0.6<km>; jikan=1<minute>};
        {kiten="東京"; shuten="銀座"; keiyu="丸ノ内線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="銀座"; shuten="霞ヶ関"; keiyu="丸ノ内線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="霞ヶ関"; shuten="国会議事堂前"; keiyu="丸ノ内線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="国会議事堂前"; shuten="赤坂見附"; keiyu="丸ノ内線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="赤坂見附"; shuten="四ツ谷"; keiyu="丸ノ内線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="四ツ谷"; shuten="四谷三丁目"; keiyu="丸ノ内線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="四谷三丁目"; shuten="新宿御苑前"; keiyu="丸ノ内線"; kyori=0.9<km>; jikan=1<minute>};
        {kiten="新宿御苑前"; shuten="新宿三丁目"; keiyu="丸ノ内線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="新宿三丁目"; shuten="新宿"; keiyu="丸ノ内線"; kyori=0.3<km>; jikan=1<minute>};
        {kiten="新宿"; shuten="西新宿"; keiyu="丸ノ内線"; kyori=0.8<km>; jikan=1<minute>};
        {kiten="西新宿"; shuten="中野坂上"; keiyu="丸ノ内線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="中野坂上"; shuten="新中野"; keiyu="丸ノ内線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="新中野"; shuten="東高円寺"; keiyu="丸ノ内線"; kyori=1.0<km>; jikan=1<minute>};
        {kiten="東高円寺"; shuten="新高円寺"; keiyu="丸ノ内線"; kyori=0.9<km>; jikan=1<minute>};
        {kiten="新高円寺"; shuten="南阿佐ヶ谷"; keiyu="丸ノ内線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="南阿佐ヶ谷"; shuten="荻窪"; keiyu="丸ノ内線"; kyori=1.5<km>; jikan=2<minute>};
        {kiten="中野坂上"; shuten="中野新橋"; keiyu="丸ノ内線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="中野新橋"; shuten="中野富士見町"; keiyu="丸ノ内線"; kyori=0.6<km>; jikan=1<minute>};
        {kiten="中野富士見町"; shuten="方南町"; keiyu="丸ノ内線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="市ヶ谷"; shuten="四ツ谷"; keiyu="南北線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="四ツ谷"; shuten="永田町"; keiyu="南北線"; kyori=1.3<km>; jikan=3<minute>};
        {kiten="永田町"; shuten="溜池山王"; keiyu="南北線"; kyori=0.9<km>; jikan=1<minute>};
        {kiten="溜池山王"; shuten="六本木一丁目"; keiyu="南北線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="六本木一丁目"; shuten="麻布十番"; keiyu="南北線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="麻布十番"; shuten="白金高輪"; keiyu="南北線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="白金高輪"; shuten="白金台"; keiyu="南北線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="白金台"; shuten="目黒"; keiyu="南北線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="市ヶ谷"; shuten="飯田橋"; keiyu="南北線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="飯田橋"; shuten="後楽園"; keiyu="南北線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="後楽園"; shuten="東大前"; keiyu="南北線"; kyori=1.3<km>; jikan=3<minute>};
        {kiten="東大前"; shuten="本駒込"; keiyu="南北線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="本駒込"; shuten="駒込"; keiyu="南北線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="駒込"; shuten="西ヶ原"; keiyu="南北線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="西ヶ原"; shuten="王子"; keiyu="南北線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="王子"; shuten="王子神谷"; keiyu="南北線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="王子神谷"; shuten="志茂"; keiyu="南北線"; kyori=1.6<km>; jikan=3<minute>};
        {kiten="志茂"; shuten="赤羽岩淵"; keiyu="南北線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="西船橋" ; shuten="原木中山"; keiyu="東西線"; kyori=1.9<km>; jikan=3<minute>};
        {kiten="原木中山"; shuten="妙典"; keiyu="東西線"; kyori=2.1<km> ; jikan=2<minute>};
        {kiten="妙典"; shuten="行徳"; keiyu="東西線"; kyori=1.3<km> ; jikan=2<minute>};
        {kiten="行徳"; shuten="南行徳"; keiyu="東西線"; kyori=1.5<km> ; jikan=2<minute>};
        {kiten="南行徳"; shuten="浦安" ; keiyu="東西線"; kyori=1.2<km> ; jikan=2<minute>};
        {kiten="浦安" ; shuten="葛西"; keiyu="東西線"; kyori=1.9<km> ; jikan=2<minute>};
        {kiten="葛西"; shuten="西葛西"; keiyu="東西線"; kyori=1.2<km> ; jikan=2<minute>};
        {kiten="西葛西"; shuten="南砂町"; keiyu="東西線"; kyori=2.7<km> ; jikan=2<minute>};
        {kiten="南砂町"; shuten="東陽町"; keiyu="東西線"; kyori=1.2<km> ; jikan=2<minute>};
        {kiten="東陽町"; shuten="木場" ; keiyu="東西線"; kyori=0.9<km> ; jikan=1<minute>};
        {kiten="木場"; shuten="門前仲町"; keiyu="東西線"; kyori=1.1<km> ; jikan=1<minute>};
        {kiten="門前仲町"; shuten="茅場町"; keiyu="東西線"; kyori=1.8<km> ; jikan=2<minute>};
        {kiten="茅場町"; shuten="日本橋"; keiyu="東西線"; kyori=0.5<km> ; jikan=1<minute>};
        {kiten="日本橋"; shuten="大手町"; keiyu="東西線"; kyori=0.8<km> ; jikan=1<minute>};
        {kiten="大手町"; shuten="竹橋"; keiyu="東西線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="竹橋"; shuten="九段下"; keiyu="東西線"; kyori=1.0<km>; jikan=1<minute>};
        {kiten="九段下"; shuten="飯田橋"; keiyu="東西線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="飯田橋"; shuten="神楽坂"; keiyu="東西線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="神楽坂"; shuten="早稲田"; keiyu="東西線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="早稲田"; shuten="高田馬場"; keiyu="東西線"; kyori=1.7<km>; jikan=3<minute>};
        {kiten="高田馬場"; shuten="落合"; keiyu="東西線"; kyori=1.9<km>; jikan=3<minute>};
        {kiten="落合"; shuten="中野"; keiyu="東西線"; kyori=2.0<km>; jikan=3<minute>};
        {kiten="新木場"; shuten="辰巳"; keiyu="有楽町線"; kyori=1.5<km>; jikan=2<minute>};
        {kiten="辰巳"; shuten="豊洲"; keiyu="有楽町線"; kyori=1.7<km>; jikan=2<minute>};
        {kiten="豊洲"; shuten="月島"; keiyu="有楽町線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="月島"; shuten="新富町"; keiyu="有楽町線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="新富町"; shuten="銀座一丁目"; keiyu="有楽町線"; kyori=0.7<km>; jikan=1<minute>};
        {kiten="銀座一丁目"; shuten="有楽町"; keiyu="有楽町線"; kyori=0.5<km>; jikan=1<minute>};
        {kiten="有楽町"; shuten="桜田門"; keiyu="有楽町線"; kyori=1.0<km>; jikan=1<minute>};
        {kiten="桜田門"; shuten="永田町"; keiyu="有楽町線"; kyori=0.9<km>; jikan=2<minute>};
        {kiten="永田町"; shuten="麹町"; keiyu="有楽町線"; kyori=0.9<km>; jikan=1<minute>};
        {kiten="麹町"; shuten="市ヶ谷"; keiyu="有楽町線"; kyori=0.9<km>; jikan=1<minute>};
        {kiten="市ヶ谷"; shuten="飯田橋"; keiyu="有楽町線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="飯田橋"; shuten="江戸川橋"; keiyu="有楽町線"; kyori=1.6<km>; jikan=3<minute>};
        {kiten="江戸川橋"; shuten="護国寺"; keiyu="有楽町線"; kyori=1.3<km>; jikan=2<minute>};
        {kiten="護国寺"; shuten="東池袋"; keiyu="有楽町線"; kyori=1.1<km>; jikan=2<minute>};
        {kiten="東池袋"; shuten="池袋"; keiyu="有楽町線"; kyori=2.0<km>; jikan=2<minute>};
        {kiten="池袋"; shuten="要町"; keiyu="有楽町線"; kyori=1.2<km>; jikan=2<minute>};
        {kiten="要町"; shuten="千川"; keiyu="有楽町線"; kyori=1.0<km>; jikan=1<minute>};
        {kiten="千川"; shuten="小竹向原"; keiyu="有楽町線"; kyori=1.0<km>; jikan=2<minute>};
        {kiten="小竹向原"; shuten="氷川台"; keiyu="有楽町線"; kyori=1.5<km>; jikan=2<minute>};
        {kiten="氷川台"; shuten="平和台"; keiyu="有楽町線"; kyori=1.4<km>; jikan=2<minute>};
        {kiten="平和台"; shuten="営団赤塚"; keiyu="有楽町線"; kyori=1.8<km>; jikan=2<minute>};
        {kiten="営団赤塚"; shuten="営団成増"; keiyu="有楽町線"; kyori=1.5<km>; jikan=2<minute>};
        {kiten="営団成増"; shuten="和光市"; keiyu="有楽町線"; kyori=2.1<km>; jikan=3<minute>};
    ]

    /// ローマ字の駅名と駅名リストを受け取り、その駅の漢字表記を文字列で返す
    /// 見つからない場合は空文字列を返す
    let rec romajiToKanji (lst: Ekimei list) (romaji: string) : string =
        match lst with
        | [] -> ""
        | first :: rest ->
            if first.romaji = romaji then first.kanji
            else romajiToKanji rest romaji

    /// 漢字の駅名2つと駅間リストを受け取り、2駅が直接繋がっている場合にその距離を返す
    let rec getEkikanKyori (lst: Ekikan list) (ekimei1: string) (ekimei2: string) : float<km> option =
        match lst with
        | [] -> None
        | first :: rest ->
            if first.kiten = ekimei1 && first.shuten = ekimei2 then Some first.kyori
            else if first.kiten = ekimei2 && first.shuten = ekimei1 then Some first.kyori
            else getEkikanKyori rest ekimei1 ekimei2

    /// ローマ字の駅名を2つを受け取り、直接繋がっている場合は「x駅からy駅まではzkmです」
    /// 繋がっていない場合は「x駅とy駅は繋がっていません」
    /// 入力されたローマ字の駅名が存在しない場合は「xという駅は存在しません」という文字列を返す
    let kyoriWoHyoji (ekimei1: string) (ekimei2: string) : string =
        let f = romajiToKanji globalEkimeiList
        let kanjiEkimei1 = f ekimei1
        let kanjiEkimei2 = f ekimei2
        if kanjiEkimei1 = "" then ekimei1 + "という駅は存在しません"
        else if kanjiEkimei2 = "" then ekimei2 + "という駅は存在しません"
        else
            match getEkikanKyori globalEkikanList kanjiEkimei1 kanjiEkimei2 with
            | None -> kanjiEkimei1 + "駅と" + kanjiEkimei2 + "駅は繋がっていません"
            | Some kyori ->kanjiEkimei1 + "駅から" + kanjiEkimei2 + "駅までは" + kyori.ToString() + "kmです"