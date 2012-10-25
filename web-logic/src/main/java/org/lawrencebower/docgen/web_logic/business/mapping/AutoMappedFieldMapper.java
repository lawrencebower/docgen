package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.business.Business;
import org.lawrencebower.docgen.web_model.view.business.BusinessView;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.List;

public class AutoMappedFieldMapper {

    private BusinessView businessView;

    public void mapCustomerFields(List<DocumentInfoView> documentInfoViews, BusinessView businessView) {

        this.businessView = businessView;

        for (DocumentInfoView documentInfoView : documentInfoViews) {
            checkAndMapDocument(documentInfoView);
        }
    }

    private void checkAndMapDocument(DocumentInfoView documentInfoView) {
        for (DocComponentView docComponentView : documentInfoView.getComponentViews()) {
            checkAndMapComponent(docComponentView);
        }
    }

    private void checkAndMapComponent(DocComponentView docComponentView) {

        boolean fieldIsAutoMapped = docComponentView.isAutoMappedField();

        if (fieldIsAutoMapped) {
            AutoMappedField autoMappedField = docComponentView.getAutoMappedField();
            mapFieldValue(docComponentView, autoMappedField);
        }
    }

    private void mapFieldValue(DocComponentView docComponentView, AutoMappedField field) {

        Business business = businessView.getCustomer();

        if (field == AutoMappedField.CUSTOMER_ADDRESS) {
            setComponentText(docComponentView, business.getAddress());
        } else if (field == AutoMappedField.CUSTOMER_CONTACT_NAME) {
            setComponentText(docComponentView, business.getContactName());
        } else if (field == AutoMappedField.CUSTOMER_COUNTRY) {
            setComponentText(docComponentView, business.getCountry());
        } else if (field == AutoMappedField.CUSTOMER_NAME) {
            setComponentText(docComponentView, business.getName());
        } else if (field == AutoMappedField.CUSTOMER_PHONE) {
            setComponentText(docComponentView, business.getPhone());
        }
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
