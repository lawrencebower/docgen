package org.lawrencebower.docgen.web_model.view.product;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.*;

public class ProductSelection {

    public static final String NO_PRODUCTS_SELECTED = "No products selected?!";

    private Map<String, ProductView> products = new LinkedHashMap<>();

    public void addProduct(ProductView product) {
        String productId = product.getProductId();
        if (products.containsKey(productId)) {
            ProductView productView = products.get(productId);
            productView.incrementQuantity();
        }else{
            products.put(productId, product);
        }
    }

    public List<ProductView> getProducts() {
        Collection<ProductView> values = products.values();
        return new ArrayList<>(values);
    }

    public boolean hasProducts(){
        return !products.isEmpty();
    }

    public void clear() {
        products.clear();
    }

    public void checkProductsSet() {
        if (products.isEmpty()) {
            throw new DocGenException(NO_PRODUCTS_SELECTED);
        }
    }
}
