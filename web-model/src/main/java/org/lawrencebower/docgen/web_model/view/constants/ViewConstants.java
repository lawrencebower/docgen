package org.lawrencebower.docgen.web_model.view.constants;

import org.lawrencebower.docgen.core.exception.DocGenException;

public class ViewConstants {

    public static final String DOCUMENT_FIELD_SEPARATOR = "~";
    public static final String CONCATENATED_FILE_NAME = "CONCATENATED_FILES.pdf";

    public enum PRODUCT_TOKEN_TYPE {
        VALUE("cost"),
        QUANTITY("quantity");

        private String name;

        PRODUCT_TOKEN_TYPE(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public static PRODUCT_TOKEN_TYPE mapFromString(String string) {

            for (PRODUCT_TOKEN_TYPE type : PRODUCT_TOKEN_TYPE.values()) {
                String typeName = type.getName();
                if (typeName.equals(string)) {
                    return type;
                }
            }

            String message = String.format("PRODUCT_TOKEN_TYPE '%s' not recognised?!", string);
            throw new DocGenException(message);
        }
    }


    public String getDocumentFieldSeparator() {
        return DOCUMENT_FIELD_SEPARATOR;
    }

    public static String getProductQuantityToken() {
        return PRODUCT_TOKEN_TYPE.QUANTITY.getName();
    }

    public static String getProductValueToken() {
        return PRODUCT_TOKEN_TYPE.VALUE.getName();
    }
}
