package org.lawrencebower.docgen.web_model.view.product;

import java.util.*;

public class ProductSelection {

    private Set<ProductView> products = new LinkedHashSet<>();

    public void addProduct(ProductView product) {
        products.add(product);
    }

    public List<ProductView> getProducts() {
        return new ArrayList<>(products);
    }
}
