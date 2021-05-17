(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** List of tuples of week timestamp, week transaction count, week ops count and
    week block count.

    There is ~1 block per minute in tezos and 10080 minutes in a week. Hence the
    content of the last columns.

    Computed using:

    https://api.tzstats.com/series/block.json?collapse=1w&columns=n_tx,n_ops,count *)
let weekly_stats =
  [
    (1529884800000, 497, 16396, 1814);
    (1530489600000, 10337, 103366, 10031);
    (1531094400000, 6444, 24425, 8478);
    (1531699200000, 6268, 41311, 9674);
    (1532304000000, 4423, 126915, 9916);
    (1532908800000, 3951, 153091, 9561);
    (1533513600000, 3709, 151066, 9088);
    (1534118400000, 5103, 164351, 9442);
    (1534723200000, 7744, 174163, 9504);
    (1535328000000, 16856, 196819, 9627);
    (1535932800000, 9080, 195198, 9756);
    (1536537600000, 11296, 200613, 9759);
    (1537142400000, 34291, 234489, 9935);
    (1537747200000, 11029, 203505, 9657);
    (1538352000000, 15221, 211566, 9718);
    (1538956800000, 12279, 212965, 9857);
    (1539561600000, 16400, 216941, 9706);
    (1540166400000, 17277, 224464, 9875);
    (1540771200000, 13674, 219879, 9855);
    (1541376000000, 17780, 222633, 9781);
    (1541980800000, 43862, 243942, 9582);
    (1542585600000, 265754, 552326, 8583);
    (1543190400000, 29911, 199036, 8446);
    (1543795200000, 46262, 255443, 9395);
    (1544400000000, 46809, 265757, 9433);
    (1545004800000, 17226, 223122, 9601);
    (1545609600000, 14758, 224245, 9671);
    (1546214400000, 20025, 234288, 9775);
    (1546819200000, 15696, 227917, 9667);
    (1547424000000, 20349, 235370, 9743);
    (1548028800000, 17173, 232767, 9725);
    (1548633600000, 17551, 234505, 9714);
    (1549238400000, 22169, 241307, 9741);
    (1549843200000, 17892, 239578, 9787);
    (1550448000000, 17899, 239569, 9784);
    (1551052800000, 23444, 244467, 9749);
    (1551657600000, 17721, 236558, 9666);
    (1552262400000, 24563, 247532, 9743);
    (1552867200000, 22835, 250750, 9835);
    (1553472000000, 23011, 251950, 9845);
    (1554076800000, 29518, 257341, 9739);
    (1554681600000, 21775, 246456, 9683);
    (1555286400000, 30670, 257784, 9771);
    (1555891200000, 27023, 251536, 9687);
    (1556496000000, 22465, 248722, 9767);
    (1557100800000, 32423, 259129, 9773);
    (1557705600000, 27606, 258295, 9859);
    (1558310400000, 24614, 252204, 9771);
    (1558915200000, 32625, 248919, 9520);
    (1559520000000, 26519, 251351, 9736);
    (1560124800000, 32499, 258754, 9683);
    (1560729600000, 33250, 257565, 9578);
    (1561334400000, 29034, 252155, 9611);
    (1561939200000, 37005, 260170, 9597);
    (1562544000000, 31342, 255413, 9618);
    (1563148800000, 27970, 250786, 9563);
    (1563753600000, 43176, 273231, 9702);
    (1564358400000, 31888, 268034, 9877);
    (1564963200000, 38565, 272631, 9781);
    (1565568000000, 40317, 269809, 9669);
    (1566172800000, 32076, 266277, 9802);
    (1566777600000, 36307, 269958, 9789);
    (1567382400000, 35313, 263821, 9683);
    (1567987200000, 30223, 264633, 9799);
    (1568592000000, 38428, 276965, 9913);
    (1569196800000, 38314, 279131, 9914);
    (1569801600000, 42121, 282076, 9921);
    (1570406400000, 32572, 272638, 9900);
    (1571011200000, 33358, 261102, 9723);
    (1571616000000, 52981, 280392, 9812);
    (1572220800000, 40154, 270988, 9876);
    (1572825600000, 61329, 296468, 9889);
    (1573430400000, 40552, 262779, 9608);
    (1574035200000, 39421, 264409, 9742);
    (1574640000000, 55234, 289723, 9931);
    (1575244800000, 46487, 284110, 9976);
    (1575849600000, 64465, 307251, 9978);
    (1576454400000, 53005, 289408, 9920);
    (1577059200000, 42832, 281309, 9975);
    (1577664000000, 56314, 294350, 9961);
    (1578268800000, 51173, 289804, 9966);
    (1578873600000, 68059, 309998, 10023);
    (1579478400000, 64670, 308314, 10001);
    (1580083200000, 71682, 314239, 10034);
    (1580688000000, 77387, 320814, 9996);
    (1581292800000, 100282, 346279, 9975);
    (1581897600000, 86670, 329707, 9957);
    (1582502400000, 76753, 311822, 9901);
    (1583107200000, 83277, 304617, 9745);
    (1583712000000, 80841, 304818, 9802);
    (1584316800000, 94173, 322773, 9905);
    (1584921600000, 71931, 300080, 9921);
    (1585526400000, 69493, 299321, 9977);
    (1586131200000, 89672, 321104, 9984);
    (1586736000000, 74397, 308355, 10054);
    (1587340800000, 100712, 336139, 10055);
    (1587945600000, 93507, 327961, 10037);
    (1588550400000, 112609, 346175, 10039);
    (1589155200000, 91704, 324183, 10033);
    (1589760000000, 102430, 335567, 10058);
    (1590364800000, 94686, 324824, 10029);
    (1590969600000, 91343, 322663, 10029);
    (1591574400000, 125174, 357268, 10032);
    (1592179200000, 95985, 329653, 10023);
    (1592784000000, 122939, 357827, 10035);
    (1593388800000, 95589, 327834, 10029);
    (1593993600000, 128419, 362984, 10032);
    (1594598400000, 122955, 359831, 10044);
    (1595203200000, 121149, 354555, 10033);
    (1595808000000, 123127, 356107, 10026);
    (1596412800000, 139315, 372670, 10004);
    (1597017600000, 157274, 393230, 10022);
    (1597622400000, 135269, 366162, 9974);
    (1598227200000, 148236, 374878, 9964);
    (1598832000000, 127456, 352065, 9963);
    (1599436800000, 151080, 373997, 9952);
    (1600041600000, 126361, 350427, 9968);
    (1600646400000, 140182, 365980, 10025);
    (1601251200000, 138945, 362958, 9987);
    (1601856000000, 125262, 348689, 10029);
    (1602460800000, 163734, 386645, 10011);
    (1603065600000, 130914, 354378, 9999);
    (1603670400000, 165121, 389401, 10033);
    (1604275200000, 138447, 361921, 10028);
    (1604880000000, 189794, 404942, 9870);
    (1605484800000, 146999, 362987, 9841);
    (1606089600000, 164426, 389926, 9989);
    (1606694400000, 168238, 392715, 10022);
    (1607299200000, 140107, 361585, 10031);
    (1607904000000, 189296, 412488, 10009);
    (1608508800000, 152377, 376164, 10008);
    (1609113600000, 198728, 420519, 9846);
    (1609718400000, 185671, 406782, 9654);
    (1610323200000, 179296, 398205, 9664);
    (1610928000000, 218730, 439105, 9667);
    (1611532800000, 172690, 392509, 9691);
    (1612137600000, 187690, 410862, 9643);
    (1612742400000, 264982, 494725, 9773);
    (1613347200000, 232473, 465874, 9975);
    (1613952000000, 273033, 504512, 9991);
    (1614556800000, 240863, 470355, 9998);
    (1615161600000, 302082, 533510, 10011);
    (1615766400000, 258743, 487761, 10004);
    (1616371200000, 344647, 569310, 9989);
    (1616976000000, 445942, 674148, 10012);
    (1617580800000, 547832, 775660, 9947);
    (1618185600000, 656723, 887993, 10012);
    (1618790400000, 811462, 1036103, 9922);
    (1619395200000, 810847, 1040980, 9952);
    (1620000000000, 273545, 359345, 3779);
  ]
