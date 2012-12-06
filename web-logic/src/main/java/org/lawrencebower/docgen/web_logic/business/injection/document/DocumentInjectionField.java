package org.lawrencebower.docgen.web_logic.business.injection.document;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.ArrayList;
import java.util.List;

public enum DocumentInjectionField {

    PRODUCT_NAME("DOCUMENT_INJECT_PRODUCT_NAME"),
    PRODUCT_VALUE("DOCUMENT_INJECT_PRODUCT_VALUE"),
    PRODUCT_ORIGIN("DOCUMENT_INJECT_PRODUCT_ORIGIN"),
    PRODUCT_QUANTITY("DOCUMENT_INJECT_PRODUCT_QUANTITY"),
    PRODUCT_MODEL("DOCUMENT_INJECT_PRODUCT_MODEL"),
    PRODUCT_TARIFF_NUMBER("DOCUMENT_INJECT_TARIFF_NUMBER"),
    PRODUCT_CUSTOMS_DESCRIPTION("DOCUMENT_INJECT_CUSTOMS_DESCRIPTION");

    private String name;

    DocumentInjectionField(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static boolean containsName(String name) {

        boolean containsName = false;

        for (DocumentInjectionField productField : DocumentInjectionField.values()) {

            String productName = productField.getName();

            if (productName.equals(name)) {
                containsName = true;
            }
        }

        return containsName;
    }

    public static List<String> getNames() {

        List<String> names = new ArrayList<>();

        for (DocumentInjectionField productField : DocumentInjectionField.values()) {
            String productName = productField.getName();
            names.add(productName);
        }

        return names;
    }

    public static DocumentInjectionField getByFieldName(String name) {

        for (DocumentInjectionField productField : DocumentInjectionField.values()) {
            String productName = productField.getName();
            if (productName.equals(name)) {
                return productField;
            }
        }

        throw new DocGenException("enum documentInjectionField not found with name " + name);
    }

}
