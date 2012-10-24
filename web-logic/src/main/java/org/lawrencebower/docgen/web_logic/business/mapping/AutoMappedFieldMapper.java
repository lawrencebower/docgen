package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.customer.Customer;
import org.lawrencebower.docgen.web_model.view.customer.CustomerView;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.List;

public class AutoMappedFieldMapper {

    private CustomerView customerView;

    public void mapCustomerFields(List<DocumentInfoView> documentInfoViews, CustomerView customerView) {

        this.customerView = customerView;

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

        Customer customer = customerView.getCustomer();

        if (field == AutoMappedField.CUSTOMER_ADDRESS) {
            setComponentText(docComponentView, customer.getAddress());
        } else if (field == AutoMappedField.CUSTOMER_CONTACT_NAME) {
            setComponentText(docComponentView, customer.getContactName());
        } else if (field == AutoMappedField.CUSTOMER_COUNTRY) {
            setComponentText(docComponentView, customer.getCountry());
        } else if (field == AutoMappedField.CUSTOMER_NAME) {
            setComponentText(docComponentView, customer.getName());
        } else if (field == AutoMappedField.CUSTOMER_PHONE) {
            setComponentText(docComponentView, customer.getPhone());
        }
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
