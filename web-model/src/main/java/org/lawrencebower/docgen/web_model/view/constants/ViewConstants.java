package org.lawrencebower.docgen.web_model.view.constants;

public class ViewConstants {

    public static final String DOCUMENT_FIELD_SEPARATOR = "~";
    public static final String PRODUCT_QUANTITY_TOKEN = "quantity";
    public static final String PRODUCT_COST_TOKEN = "cost";
    public static final String CONCATENATED_FILE_NAME = "CONCATENATED_FILES.pdf";

    public String getDocumentFieldSeparator() {
        return DOCUMENT_FIELD_SEPARATOR;
    }

    public static String getProductQuantityToken() {
        return PRODUCT_QUANTITY_TOKEN;
    }

    public static String getProductCostToken() {
        return PRODUCT_COST_TOKEN;
    }
}
