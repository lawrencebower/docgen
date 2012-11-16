package org.lawrencebower.docgen.web_model.view.product;

import java.util.*;

public class ProductSelection {

    private Map<String, ProductView> products = new LinkedHashMap<>();

    public void addProduct(Product product) {
        String productId = product.getProductId();
        if (products.containsKey(productId)) {
            ProductView productView = products.get(productId);
            productView.incrementQuantity();
        }else{
            products.put(productId, new ProductView(product));
        }
    }

    public List<ProductView> getProducts() {
        Collection<ProductView> values = products.values();
        return new ArrayList<>(values);
    }
}
