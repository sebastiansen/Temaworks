(ns temaworks.meta.data
  (:use temaworks.meta.types)
  (:import [temaworks.meta.types Entity-type Attribute Reference Relationship]))

(declare city region client policy company policy-sub-type policy-type item)

(defn city-has-region
  []
  (Relationship.
    #(city) #(region) true true :many-to-one {:region_idregion :idregion} nil))

(defn client-has-city
  []
  (Relationship.
    #(client) #(city) false true :many-to-one {:comuna_idcomuna :idcomuna :comuna_region_idregion :region_idregion} nil))

(defn policy-has-client
  []
  (Relationship.
    #(policy) #(client) false true :many-to-one {:cliente_rut :rut} nil))

(defn policy-has-company
  []
  (Relationship.
    #(policy) #(company) true true :many-to-one {:compania_nombre :nombre} nil))

(defn policy-sub-type-has-policy-type 
  []
  (Relationship.
    #(policy-sub-type) #(policy-type) true true :many-to-one {:ramo_ramo :ramo} nil))

(defn policy-has-policy-sub-type 
  []
  (Relationship.
    #(policy) #(policy-sub-type) false true :many-to-one {:subramo_subramo :subramo :subramo_ramo_ramo :ramo_ramo} nil))

(defn item-has-policy 
  []
  (Relationship.
    #(item) #(policy) true true :many-to-one {:poliza_numero :numero :poliza_compania_nombre :compania_nombre} nil))

(defn item-has-client 
  []
  (Relationship.
    #(item) #(client) false true :many-to-one {:cliente_rut :rut} nil))

(defn policy-sub-type
  []
  (Entity-type. 
    :subramo "Sub ramo" "Sub ramos"
    {:subramo 
     (Attribute.
       "Nombre" String :textbox true true false nil)}
    {:ramo
     (Reference.
       "Ramo"
       #(policy-sub-type-has-policy-type)
       :from
       :combo-box
       nil)}
    false nil nil nil :ramo 
    #(str (:subramo %))
    nil
    false
    nil))

(defn policy-type
  []
  (Entity-type. 
    :ramo "Ramo" "Ramo"
    {:ramo 
     (Attribute.
       "Nombre" String :textbox true true false nil)}
    {}
    false nil nil nil nil 
    #(str (:ramo %))
    nil
    false
    nil))

(defn client
  []
  (Entity-type. 
    :cliente "Cliente" "Clientes"
    {:rut 
     (Attribute.
       "RUT"  String :textbox true true false nil)
     :nombres 
     (Attribute.
       "Nombres" String :textbox false true false nil)
     :apellidos
     (Attribute.
       "Apellidos" String :textbox false true false nil)
     :sexo 
     (Attribute.
       "Sexo" String :multi-option false false false {:orient "horizontal" :default-value "Masculino" :widget-type :radiogroup :options ["Masculino" "Femenino"]})
     :estado_civil 
     (Attribute.
       "Estado Civíl" String :multi-option false false false {:orient "horizontal" :default-value "Casado(a)" :widget-type :radiogroup :options ["Casado(a)" "Soltero(a)"]})
     :movil   
     (Attribute.
       "Móvil" String :textbox false false false nil)
     :fono_casa   
     (Attribute.
       "Fono Casa" String :textbox false false false nil)
     :fono_trabajo   
     (Attribute.
       "Fono Trabajo" String :textbox false false false nil)
     :direccion   
     (Attribute.
       "Dirección" String :textbox false false false nil)
     :email   
     (Attribute.
       "E-mail" String :textbox false false false nil 
       ;{:actions [{:label "Enviar correo" 
       ;            :icon :email 
       ;            :function (fn [butt value] (.setHref butt (str "mailto:" value)))}]}
       )}
    {:comuna
     (Reference.
       "Comuna"
       #(client-has-city)
       :from
       :combo-box
       nil)}   
    true
    [:rut :nombres :apellidos :sexo :estado_civil :comuna :direccion :movil :fono_casa :fono_trabajo :email] 
    [:rut :nombres :apellidos :movil :fono_trabajo :email]
    [:rut :nombres :apellidos :comuna]
    nil
    #(str (:nombres %) " " (:apellidos %))
    nil
    false
    :group))

(defn company
  []
  (Entity-type. 
    :compania "Compañía" "Compañías"
    {:nombre 
     (Attribute.
       "Nombre" String :textbox true true false nil)
     :contacto 
     (Attribute.
       "Contacto" String :textbox false false false nil)
     :telefonos 
     (Attribute.
       "Telefonos" String :textbox false false false nil)
     :direccion 
     (Attribute.
       "Dirección" String :textbox false false false nil)}
    {}
    true
    [:nombre :contacto :telefonos :direccion]
    [:nombre :contacto :telefonos :direccion]
    []
    nil
    #(str (:nombre %))
    nil
    false
    :building))

(defn policy
  []
  (Entity-type. 
    :poliza "Póliza" "Pólizas"
    {:numero 
     (Attribute.
       "Número" Long :longbox true true false {:format (:number input-formats)})
     :fecha
     (Attribute.
       "Fecha" java.util.Date :datebox false false false nil)}
    {:cliente
     (Reference.
       "Cliente"
       #(policy-has-client)
       :from
       :ref-box
       {:mutual-ref :polizas})
     :compania
     (Reference.
       "Compañía"
       #(policy-has-company)
       :from
       :combo-box
       nil)
     :subramo
     (Reference.
       "Sub Ramo"
       #(policy-has-policy-sub-type)
       :from
       :combo-box
       nil)
     :items
     (Reference.
       "Items"
       #(item-has-policy)
       :to
       :selector
       {:mutual-ref :poliza})}
    true
    [:numero :compania :subramo :cliente :items :fecha]
    [:numero :cliente :fecha]
    []
    nil
    #(str (:numero %) " RUT Cliente " (:rut_cliente %))
    nil
    false
    :briefcase))

(defn city
  []
  (Entity-type. 
    :comuna "Comuna" "Comunas"
    {:idcomuna 
     (Attribute.
       "Número" Long nil true true false nil)
     :nombre 
     (Attribute.
       "Nombre" String :textbox false true false nil)}
    {:region
     (Reference.
       "Región"
       #(city-has-region)
       :from
       :ref-box
       nil)}
    false nil nil nil :region 
    #(str (:nombre %))
    nil
    false
    nil))

(defn region
  []
  (Entity-type. 
    :region "Región" "Región"
    {:idregion 
     (Attribute.
       "Número" Long nil true true false nil)
     :nombre 
     (Attribute.
       "Nombre" String :textbox false true false nil)}
    {}
    false nil nil nil nil 
    #(str (:nombre %))
    nil
    false
    nil))

(defn item
  []
  (Entity-type. 
    :item "Item" "Items"
    {:iditem 
     (Attribute.
       "Iditem" Long nil true true false nil)
     :fecha_pago
     (Attribute.
       "Fecha de pago" java.util.Date :datebox false true false nil)
     :cuotas
     (Attribute.
       "N° cuotas" Integer :intbox false true false nil)
     :tipo_pago
     (Attribute.
       "Tipo de pago" String :multi-option false true false {:orient "vertical" :default-value "Cheques" :widget-type :radiogroup :options ["Cheques" "Tarjeta de crédito"]})
     :accesorios
     (Attribute.
       "Accesorios" String :textarea false true false nil)
     :coberturas
     (Attribute.
       "Coberturas" String :textarea false true false nil)
     :deducible
     (Attribute.
       "Deducible" Integer :intbox false true false nil)
     :monto_asegurado
     (Attribute.
       "Monto asegurado" Integer :intbox false true false nil)
     :prima_neta
     (Attribute.
       "Prima neta" Integer :intbox false true false nil)
     :prima_excenta
     (Attribute.
       "Prima excenta" Integer :intbox false true false nil)
     :prima_afecta
     (Attribute.
       "Prima afecta" Integer :intbox false true false nil)
     :iva
     (Attribute.
       "Iva" Integer :intbox false true false nil)
     :prima_total
     (Attribute.
       "Prima total" Integer :intbox false true false nil)}
    {:poliza
     (Reference.
       "Póliza" 
       #(item-has-policy)
       :from
       :ref-box
       {:mutual-ref :items})
     :client
     (Reference.
       "Cliente responsable de pago" 
       #(item-has-client)
       :from
       :ref-box
       nil)}
    false 
    [:poliza :client :fecha_pago :cuotas :tipo_pago :accesorios :coberturas :deducible :monto_asegurado :prima_neta :prima_excenta :prima_afecta :iva :prima_total] 
    [:fecha_pago :accesorios :coberturas] 
    nil nil 
    #(str (:nombre %))
    nil
    true
    nil))
  

(def types [(trampoline #(client)) (trampoline #(policy)) (trampoline #(company))])

(def initial-view {:view-type :selector :entity-type (trampoline #(client))})

(def app-name "Sistema Gestión Pólizas Generales y Vida")
