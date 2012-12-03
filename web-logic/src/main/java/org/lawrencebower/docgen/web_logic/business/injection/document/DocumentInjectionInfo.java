package org.lawrencebower.docgen.web_logic.business.injection.document;

import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

public class DocumentInjectionInfo {

    private ProductView product;

    public DocumentInjectionInfo(ProductView product) {
        this.product = product;
    }

    public String getFieldValueByName(String fieldName) {
        DocumentInjectionField injectionField = DocumentInjectionField.getByFieldName(fieldName);
        return getFieldValueByType(injectionField);
    }

    public String getFieldValueByType(DocumentInjectionField field) {

        String value = "";

        switch (field) {
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
            case PRODUCT_MODEL:
                value = product.getId();
                break;
        }

        return value;
    }

    public void setNameExtension(DocumentView documentView) {
        String productId = product.getId();
        documentView.setNameExtension(productId);
    }
}
