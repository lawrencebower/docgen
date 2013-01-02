package org.lawrencebower.docgen.web_logic.view.document;

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
            case PRODUCT_TARIFF_NUMBER:
                value = product.getHarmonizedTariffNumber();
                break;
            case PRODUCT_CUSTOMS_DESCRIPTION:
                value = product.getCustomsDescription();
                break;
        }

        return value;
    }

    public void setDocumentNameExtension(DocumentView documentView) {
        String productId = product.getId();
        documentView.setNameExtension(productId);
    }
}
