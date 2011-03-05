(ns temaworks.handling.db)

(def db
     {:classname   "com.mysql.jdbc.Driver"
      :subprotocol "mysql"
      :user        "root"
      :password    "grtrps"
      :auto-commit true                                    
      :subname     "//localhost:3306/buses"})