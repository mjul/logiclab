(ns logiclab.kontoplan
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn alle-transaktionstyper
  [tx]
  (membero tx ["Ordinær" "Annullering"]))

(defn debet
  [dk]
  (== dk "Debet"))

(defn kredit
  [dk]
  (== dk "Kredit"))

(defn debet-eller-kredit
  [dk]
  (conde
   ((debet dk))
   ((kredit dk))))

(defn alle-regnskabskredse
  [rk]
  (membero rk ["GRA" "KP" "STK"]))


(defn alle-ydelsesnavne
  [y]
  (membero y ["Alderssum" "Genkøb" "Reserveoverførsel/I" "Reserveoverførsel/U"
              "Tvangsophævelse" "TvangsophævelseUnder100Kr"]))

(defn selskabo [u]
  (== u "100"))

(defn stedo [u]
  (membero u ["380" "610"]))

(defn beloebstypeo [b]
  (membero b ["GenkøbUnder100Kr"
              "IaltTilUdbetaling/Overførsel"
              "IaltTilUdbetaling/OverførselAutogen"
              "OpsparetBonusMedAfgift"
              "OpsparetBonusUdenAfgift"
              "OverførtPAL"
              "OverførtPAL(Markedsrente)"
              "RenterVedrOvfAftale"
              "Reserveoverførselsbeløb"
              "SkyldigAfgift"
              "SkyldigAfgiftAutogen"
              "TvangsophævelseMellem100KrOg5000Kr"
              "TvangsophævelseUnder100Kr"
              "VærdiAfAlderssum"
              "VærdiAfPension"
              "VærdiAfRatePension"]))

(defn beloebstype-for-likviditeto
  [b]
  (membero b ["IaltTilUdbetaling/Overførsel"
              "IaltTilUdbetaling/OverførselAutogen"
              "SkyldigAfgift"
              "SkyldigAfgiftAutogen"]))

(defn- tabelo-clause
  [u v]
  (if (symbol? v)
    (list v u)
    (list '== u v)))

(defmacro tabelo
  "Definer mulige kombinationer af logiske variable ud fra en tabel.
   To argumenter: en liste af logiske variable, og en liste af lister med
   tilladte kombinationer."
  [lvs tabel]
  (let [unifications (for [r tabel]
                       (map tabelo-clause lvs r))]
    `(conde
      ~@unifications)))


(defn gyldigt-bogfoerende-system-og-beloebstype
  [bogfoerende-system beloebstype]
  (conde
   ((== bogfoerende-system "SPUF")
    (membero beloebstype ["IaltTilUdbetaling/Overførsel" "IaltTilUdbetaling/OverførselAutogen"]))
   ((== bogfoerende-system "SPUF")
    (membero beloebstype ["SkyldigAfgift" "SkyldigAfgiftAutogen"]))
   ((== bogfoerende-system "Udbet.service"))))


(defn gyldigt-initierende-system-og-ydelsesnavn
  "Relation: hvilke systemer der sender hvilke ydelsesnavne."
  [initierende-system ydelsesnavn]
  (conde
   ((membero ydelsesnavn ["Alderssum" "Genkøb" "Tvangsophævelse" "TvangsophævelseUnder100Kr"])
    (membero initierende-system ["Føniks GRA" "Føniks KP" "Føniks STK"]))
   ((membero ydelsesnavn ["Reserveoverførsel/I" "Reserveoverførsel/U"])
    (== initierende-system "WorkFlow"))))

(defn gyldigt-initierende-system-og-regnskabskreds
  "Relation: Sammenhænge mellem initierende systemer og regnskabskredse."
  [initierende-system regnskabskreds]
  (tabelo [initierende-system regnskabskreds]
          [["Føniks GRA"      "GRA"]
           ["Føniks KP"       "KP" ]
           ["Føniks STK"      "STK"]
           ["WorkFlow"        alle-regnskabskredse]]))

(defn kontobro
  []
  (run* [q]
        (fresh [regnskabskreds transaktionstype ydelsesnavn bogfoerende-system initierende-system beloebstype debetkredit
                selskab sted art formaal type kunder]

               ;; Gyldige bilag (inputs)
               (gyldigt-bogfoerende-system-og-beloebstype bogfoerende-system beloebstype)
               (gyldigt-initierende-system-og-ydelsesnavn initierende-system ydelsesnavn)
               (gyldigt-initierende-system-og-regnskabskreds initierende-system regnskabskreds)

               ;; Gyldige konti (outputs)
               (selskabo selskab)
               (stedo sted)
               (== formaal "000")
               
               (conde
                
                (;; Samme konto for ydelsesinterim på tværs af alle selskaber
                 (== beloebstype "YdelsesInterim")
                 (alle-transaktionstyper transaktionstype)
                 (debet-eller-kredit debetkredit)
                 (== bogfoerende-system "Udbet.service")
                 (== sted "610")
                 (== art "65202")
                 )
                
                ((== beloebstype "GenkøbUnder100Kr")
                 (alle-transaktionstyper transaktionstype)
                 (alle-regnskabskredse regnskabskreds)
                 (debet-eller-kredit debetkredit)
                 (== ydelsesnavn "TvangsophævelseUnder100Kr")
                 (== sted "610")
                 (== art "11305")
                 (== type "0")
                 (== kunder "7002")
                 )
                
                (;; I alt til udbetaling
                 (== beloebstype "IaltTilUdbetaling/Overførsel")
                 (alle-transaktionstyper transaktionstype)
                 (alle-regnskabskredse regnskabskreds)
                 (debet-eller-kredit debetkredit)
                 (== art "63710")
                 (tabelo [bogfoerende-system sted  type]
                         [["SPUF"            "380" "85010"]
                          ["Udbet.service"   "610" "88994"]])
                 )

                (;; Opsparet bonus med afgift
                 (== beloebstype "OpsparetBonusMedAfgift")
                 (alle-transaktionstyper transaktionstype)
                 (alle-regnskabskredse regnskabskreds)
                 (debet-eller-kredit debetkredit)
                 (== sted "610")
                 (== kunder "7002")
                 (tabelo [ydelsesnavn art]
                         [["Alderssum" "11302"]
                          ["Genkøb"    "11308"]])
                 )
                )

               
               (== q
                   {:fra {:regnskabskreds regnskabskreds
                          :transaktionstype transaktionstype
                          :ydelsesnavn ydelsesnavn
                          :initierende-system initierende-system
                          :bogfoerende-system bogfoerende-system
                          :beloebstype beloebstype
                          :debetkredit debetkredit
                          }
                    :til {:selskab selskab, :sted sted, :art art, :formaal formaal,
                          :type type, :kunder kunder}})
               )))

(comment
  (let [k (kontobro)]
    (doseq [linie (sort-by #((juxt :regnskabskreds :transaktionstype :ydelsesnavn) (:fra %)) k)]
      (println ((juxt :regnskabskreds :transaktionstype :ydelsesnavn
                      :initierende-system :bogfoerende-system :beloebstype :debetkredit)
                (:fra linie)))
      (println (:til linie))
      (println "--------------------"))
    (println "Count = " (count k)))
  )


