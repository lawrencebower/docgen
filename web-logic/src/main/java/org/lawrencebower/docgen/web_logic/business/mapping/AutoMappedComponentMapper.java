package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.web_logic.view.contact.Contact;
import org.lawrencebower.docgen.web_logic.view.document_info.DocComponentView;

import java.util.List;

public class AutoMappedComponentMapper {

    private Contact customer;
    private Contact vendor;
    private Contact business;

    public void mapComponents(List<DocComponentView> components,
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

        boolean componentIsAutoMapped = docComponentView.getAutoMappedComponent();

        if (componentIsAutoMapped) {
            AutoMappedComponent autoMappedComponent = docComponentView.getAutoMappedField();
            mapComponentValue(docComponentView, autoMappedComponent);
        }
    }

    private void mapComponentValue(DocComponentView docComponentView, AutoMappedComponent component) {

        mapCustomerFields(docComponentView, component);

        mapBusinessFields(docComponentView, component);

        mapVendorFields(docComponentView, component);
    }

    private void mapVendorFields(DocComponentView docComponentView,
                                 AutoMappedComponent component) {

        if (component == AutoMappedComponent.VENDOR_ADDRESS) {
            setComponentText(docComponentView, vendor.getAddress());
        } else if (component == AutoMappedComponent.VENDOR_CONTACT_NAME) {
            setComponentText(docComponentView, vendor.getContactName());
        } else if (component == AutoMappedComponent.VENDOR_COUNTRY) {
            setComponentText(docComponentView, vendor.getCountry());
        } else if (component == AutoMappedComponent.VENDOR_NAME) {
            setComponentText(docComponentView, vendor.getName());
        } else if (component == AutoMappedComponent.VENDOR_PHONE) {
            setComponentText(docComponentView, vendor.getPhone());
        } else if (component == AutoMappedComponent.VENDOR_EMAIL) {
            setComponentText(docComponentView, vendor.getEmail());
        } else if (component == AutoMappedComponent.VENDOR_TAX_ID) {
            setComponentText(docComponentView, vendor.getTaxId());
        }
    }

    private void mapBusinessFields(DocComponentView docComponentView,
                                   AutoMappedComponent component) {

        if (component == AutoMappedComponent.BUSINESS_ADDRESS) {
            setComponentText(docComponentView, business.getAddress());
        } else if (component == AutoMappedComponent.BUSINESS_CONTACT_NAME) {
            setComponentText(docComponentView, business.getContactName());
        } else if (component == AutoMappedComponent.BUSINESS_COUNTRY) {
            setComponentText(docComponentView, business.getCountry());
        } else if (component == AutoMappedComponent.BUSINESS_NAME) {
            setComponentText(docComponentView, business.getName());
        } else if (component == AutoMappedComponent.BUSINESS_PHONE) {
            setComponentText(docComponentView, business.getPhone());
        }
    }

    private void mapCustomerFields(DocComponentView docComponentView,
                                   AutoMappedComponent component) {

        if (component == AutoMappedComponent.CUSTOMER_ADDRESS) {
            setComponentText(docComponentView, customer.getAddress());
        } else if (component == AutoMappedComponent.CUSTOMER_CONTACT_NAME) {
            setComponentText(docComponentView, customer.getContactName());
        } else if (component == AutoMappedComponent.CUSTOMER_COUNTRY) {
            setComponentText(docComponentView, customer.getCountry());
        } else if (component == AutoMappedComponent.CUSTOMER_NAME) {
            setComponentText(docComponentView, customer.getName());
        } else if (component == AutoMappedComponent.CUSTOMER_PHONE) {
            setComponentText(docComponentView, customer.getPhone());
        }
    }

    private void setComponentText(DocComponentView docComponent, String text) {
        docComponent.setComponentValue(text);
    }

}
