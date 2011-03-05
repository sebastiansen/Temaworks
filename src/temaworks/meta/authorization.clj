(ns temaworks.meta.authorization
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship]))

(def temaworks-user
  (Entity-type.
   :temaworks_user "Usuario" "Usuarios"
    
    [(Attribute.
       "Usuario" :user  String :textbox true true false nil)
     (Attribute.
       "Contraseña" :pass  String :textbox false true false nil)]
    []   
    false
    [] 
    []
    []
    nil
    #("")
    nil
    false
    :user))
