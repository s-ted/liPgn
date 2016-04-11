(ns fr.tedoldi.lichess.game.retriever.game
  (:require
    [environ.core :as env]
    [clojure.string :as str]

    [clojure.term.colors :as color]

    [clj-time.coerce :as c]
    [clj-time.format :as f]

    [fr.tedoldi.lichess.game.retriever.console :as console]
    [fr.tedoldi.lichess.game.retriever.lichess :as lichess]
    [fr.tedoldi.lichess.game.retriever.orientdb :as dal]))

(declare username->games)

(defn last-game-timestamp [dal username]
  (:timestamp (dal/username->last-game dal username)))

(defn update-user [dal url username]
  (-> (str "Updating user " username " from server.\n")
      color/blue
      console/print-err)

  (let [f (if (dal/find-by-id dal "user" username)
            dal/update!
            dal/create-with-id!)]

    (->> username
        (lichess/username->user url)
        (f dal "user" username))

    (let [last-game-timestamp (last-game-timestamp dal
                                                   username)]
      (-> (str "Retrieving user " username " games from server since "
               (if last-game-timestamp
                 (->> last-game-timestamp
                      c/from-long
                      (f/unparse
                        (f/formatters :basic-date-time)))
                 "day one")
               ".\n")
          color/blue
          console/print-err)

      (let [games (lichess/username->games url
                                           username
                                           last-game-timestamp)
            total (count games)
            counter (atom 0)]
        (-> (str "Found " total " new games.\nCrunching data, this may take a while...\n")
            color/blue
            console/print-err)

        (doseq [{:keys [id] :as game} games]
          (let [f (if (dal/find-by-id dal "game" id)
                    dal/update!
                    dal/create-with-id!)]
            (f dal "game" id (assoc game
                                    :userId username))
            (swap! counter inc)
            (if (= (mod (deref counter) 500) 0)
              (-> (str "Crunched " (deref counter) "/" total " games...\n")
                  color/blue
                  console/print-err))
            ))))))

(defn username->user [dal username]
  (dal/find-by-id dal "user" username))

(defn username->games
  ([dal username]
   (dal/username->games dal username))

  ([dal username custom-filter]
   (->> (username->games dal username)
        (filter custom-filter))))
