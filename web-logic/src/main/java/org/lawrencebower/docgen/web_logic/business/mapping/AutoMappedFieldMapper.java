package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.business.Contact;
import org.lawrencebower.docgen.web_model.view.business.ContactView;
import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.List;

public class AutoMappedFieldMapper {

    private ContactView customerView;
    private ContactView vendorView;

    public void mapFields(List<DocumentInfoView> documentInfoViews,
                          ContactView selectedCustomer,
                          ContactView vendor) {

        this.customerView = selectedCustomer;
        this.vendorView = vendor;

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

        Contact customer = customerView.getContact();
        Contact vendor = vendorView.getContact();

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
        }else if (field == AutoMappedField.VENDOR_ADDRESS) {
            setComponentText(docComponentView, vendor.getAddress());
        } else if (field == AutoMappedField.VENDOR_CONTACT_NAME) {
            setComponentText(docComponentView, vendor.getContactName());
        } else if (field == AutoMappedField.VENDOR_COUNTRY) {
            setComponentText(docComponentView, vendor.getCountry());
        } else if (field == AutoMappedField.VENDOR_NAME) {
            setComponentText(docComponentView, vendor.getName());
        } else if (field == AutoMappedField.VENDOR_PHONE) {
            setComponentText(docComponentView, vendor.getPhone());
        } else if (field == AutoMappedField.VENDOR_EMAIL) {
            setComponentText(docComponentView, vendor.getEmail());
        } else if (field == AutoMappedField.VENDOR_TAX_ID) {
            setComponentText(docComponentView, vendor.getTaxId());
        }else{
            throw new DocGenException("AutoMappedField not recognized - " + field);
        }
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
