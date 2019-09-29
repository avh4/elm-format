module ElmFormat.Mapping where


class MapNamespace ns1 ns2 a1 a2 where
    mapNamespace :: (ns1 -> ns2) -> a1 -> a2

-- TODO: add MapAnnotation
