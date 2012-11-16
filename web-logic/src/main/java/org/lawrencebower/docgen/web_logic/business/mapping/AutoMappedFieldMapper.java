package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.constants.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.List;

public class AutoMappedFieldMapper {

    private Contact customer;
    private Contact vendor;
    private Contact business;

    public void mapFields(List<DocComponentView> components,
                          Contact customer,
                          Contact vendor,
                          Contact business) {

        this.customer = customer;
        this.vendor = vendor;
        this.business = business;

        for (DocComponentView component : components) {
            checkAndMapComponent(component);
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

        mapCustomerFields(docComponentView, field);

        mapBusinessFields(docComponentView, field);

        mapVendorFields(docComponentView, field);
    }

    private void mapVendorFields(DocComponentView docComponentView,
                                 AutoMappedField field) {

        if (field == AutoMappedField.VENDOR_ADDRESS) {
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
        }
    }

    private void mapBusinessFields(DocComponentView docComponentView,
                                   AutoMappedField field) {

        if (field == AutoMappedField.BUSINESS_ADDRESS) {
            setComponentText(docComponentView, business.getAddress());
        } else if (field == AutoMappedField.BUSINESS_CONTACT_NAME) {
            setComponentText(docComponentView, business.getContactName());
        } else if (field == AutoMappedField.BUSINESS_COUNTRY) {
            setComponentText(docComponentView, business.getCountry());
        } else if (field == AutoMappedField.BUSINESS_NAME) {
            setComponentText(docComponentView, business.getName());
        } else if (field == AutoMappedField.BUSINESS_PHONE) {
            setComponentText(docComponentView, business.getPhone());
        }
    }

    private void mapCustomerFields(DocComponentView docComponentView,
                                   AutoMappedField field) {

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
