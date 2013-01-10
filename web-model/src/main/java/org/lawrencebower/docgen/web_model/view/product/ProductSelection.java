package org.lawrencebower.docgen.web_model.view.product;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product.ProductMapper;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

public class ProductSelection {

    public static final String NO_PRODUCTS_SELECTED = "No products selected?!";

    @Autowired(required = false)
    private ProductMapper productMapper;

    private Map<String, ProductView> products = new LinkedHashMap<>();

    private ProductSelection() {//force spring creation
    }

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

    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap) {
        for (ProductView productView : getProducts()) {
            productMapper.mapFieldValuesToProduct(parameterMap, productView);
        }
    }
}
