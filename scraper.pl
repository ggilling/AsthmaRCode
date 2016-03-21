#!/usr/bin/perl

$PASSWORD = Gasthma:1984nove

for (my $i = 1; $i < 1000000; $i++) {
   eval {
       system("curl
\'https://stream.twitter.com/1/statuses/filter.json?track=\@Asthma,
Accuneb,
Aerobid,
albuterol,
Alvesco,
ashtma,
asma,
Asmanex,
asmtha,
asth,
asthama,
asthamatic,
asthamtic,
Asthma,
asthma,
asthma,
asthmaticus,
asthmic,
astma,
asyhma,
azth,
ashtma,
asma,
asma,
Asmanex,
asmtha,
asth,
asthama,
asthamatic,
asthamtic,
Asthma,
asthma,
asthmaticus,
asthmic,
astma,
asyhma,
azth,
Foradil,
inhaler,
nebulizer,
Performist,
Primatene,
Proair,
Proventil,
puffer,
Pulmicort,
Qvar,
salbutamol,
Serevent,
singulair,
Theophylline,
twisthaler,
Uniphyl,
Ventolin,
weesing,
weezing,
wheeszing,
wheez,
wheeze,
wheezed,
wheezing,
wheezy,
whez,
whezzing,
Xopenex,
Zyflo&lang=en\'
 -u $PASSWORD >> asthmatweets.json ");
       1;
   };
   sleep(300);
   next;
}
~



