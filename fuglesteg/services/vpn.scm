(define-module (fuglesteg services vpn)
  #:use-module (gnu)
  #:use-module (gnu services vpn))

(define-public peer-k8
  (wireguard-peer
   (name "K8")
   (endpoint "k8.fuglesteg.net:51820")
   (public-key "Z2Inejk/94vVSbaEwJm5y+undnfOR8ADky9zidVTHEU=")
   (allowed-ips '("10.0.0.8/32"))
   (keep-alive 25)))

(define-public peer-k80
  (wireguard-peer
   (name "K80")
   (endpoint "k80.fuglesteg.net:51820")
   (public-key "RmEhWSJduXjOXdrF4C+Omu6yGiebPGHpNTWGdXNYJQQ=")
   (allowed-ips '("10.0.0.80/32"))
   (keep-alive 25)))

(define-public peer-kip
  (wireguard-peer
   (name "Kip")
   (public-key "BOvo9eyx9yZd4cyZQ45nYj+LqO4DqHYLjVHoD1AxsyM=")
   (allowed-ips '("10.0.0.1/32"))
   (keep-alive 25)))

(define* (fuglesteg-wireguard-service #:key private-key address peers)
  (service wireguard-service-type
           (wireguard-configuration
            (interface "wg-fuglesteg")
            (private-key private-key)
            (addresses (list address))
            (peers peers))))

(export fuglesteg-wireguard-service)

(define-public (fuglesteg-hosts-service)
  (simple-service 'fuglesteg-hosts
                  hosts-service-type
                  (list (host "10.0.0.8" "k8")
                        (host "10.0.0.80" "k80")
                        (host "10.0.0.1" "kip"))))
