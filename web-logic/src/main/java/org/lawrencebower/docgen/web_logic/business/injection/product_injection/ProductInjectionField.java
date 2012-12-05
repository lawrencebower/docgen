package org.lawrencebower.docgen.web_logic.business.injection.product_injection;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public enum ProductInjectionField {

    PRODUCT_NAME("PRODUCT_INJECT_NAME"),
    PRODUCT_VALUE("PRODUCT_INJECT_VALUE"),
    PRODUCT_ORIGIN("PRODUCT_INJECT_ORIGIN"),
    PRODUCT_QUANTITY("PRODUCT_INJECT_QUANTITY"),
    PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION("PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION"),
    PRODUCT_HARMONIZED_TARIFF_DESCRIPTION("PRODUCT_HARMONIZED_TARIFF_DESCRIPTION");

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

            String productName = productField.getName();

            if (productName.equals(name)) {
                containsName = true;
            }
        }

        return containsName;
    }

    public static List<String> getNames() {

        List<String> names = new ArrayList<>();

        for (ProductInjectionField productField : ProductInjectionField.values()) {
            String productName = productField.getName();
            names.add(productName);
        }

        return names;
    }

    public static ProductInjectionField getByFieldName(String name) {

        for (ProductInjectionField productField : ProductInjectionField.values()) {
            String productName = productField.getName();
            if (productName.equals(name)) {
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
            case PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION:
                value = product.getCommercialInvoiceDescription();
                break;
            case PRODUCT_HARMONIZED_TARIFF_DESCRIPTION:
                value = product.getHarmonizedTariffNumber();
                break;
            default:
                String message = String.format("ProductInjectionField '%s' not mapped", productField);
                throw new DocGenException(message);
        }

        return value;
    }
}
