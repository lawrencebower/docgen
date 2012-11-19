package org.lawrencebower.docgen.web_logic.view.constants;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

public enum ProductInjectionField {

    PRODUCT_NAME("PRODUCT_INJECT_NAME"),
    PRODUCT_VALUE("PRODUCT_INJECT_VALUE"),
    PRODUCT_ORIGIN("PRODUCT_INJECT_ORIGIN"),
    PRODUCT_QUANTITY("PRODUCT_INJECT_QUANTITY");

    private String name;

    ProductInjectionField(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static boolean containsName(String name) {

        boolean containsName = false;

        for (ProductInjectionField productField : ProductInjectionField.values()) {
            if (productField.getName().equals(name)) {
                containsName = true;
            }
        }

        return containsName;
    }

    public static ProductInjectionField getByFieldName(String name) {
        for (ProductInjectionField productField : ProductInjectionField.values()) {
            if (productField.getName().equals(name)) {
                return productField;
            }
        }
        throw new DocGenException("enum productInjectionField not found with name " + name);
    }

    public static String getProductFieldByType(ProductInjectionField productField, ProductView product) {

        String value = "";

        switch (productField) {
            case PRODUCT_NAME:
                value = product.getProductName();
                break;
            case PRODUCT_ORIGIN:
                value = product.getProductCountryOfOrigin();
                break;
            case PRODUCT_VALUE:
                value = product.getProductValue();
                break;
            case PRODUCT_QUANTITY:
                value = product.getQuantityString();
                break;
        }

        return value;
    }
}
