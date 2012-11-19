package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.web_logic.view.contact.Contact;

public class AutoMappedComponentInfo {

    private Contact customer;
    private Contact vendor;
    private Contact business;

    public AutoMappedComponentInfo(Contact customer,
                                   Contact vendor,
                                   Contact business) {

        this.customer = customer;
        this.vendor = vendor;
        this.business = business;
    }


    public String getCustomerName() {
        return customer.getName();
    }

    public String getCustomerContactName() {
        return customer.getContactName();
    }

    public String getCustomerPhone() {
        return customer.getPhone();
    }

    public String getCustomerCountry() {
        return customer.getCountry();
    }

    public String getCustomerAddress() {
        return customer.getAddress();
    }

    public String getBusinessName() {
        return business.getName();
    }

    public String getBusinessContactName() {
        return business.getContactName();
    }

    public String getBusinessPhone() {
        return business.getPhone();
    }

    public String getBusinessCountry() {
        return business.getCountry();
    }

    public String getBusinessAddress() {
        return business.getAddress();
    }

    public String getVendorName() {
        return vendor.getName();
    }

    public String getVendorContactName() {
        return vendor.getContactName();
    }

    public String getVendorPhone() {
        return vendor.getPhone();
    }

    public String getVendorCountry() {
        return vendor.getCountry();
    }

    public String getVendorAddress() {
        return vendor.getAddress();
    }

    public String getVendorEmail() {
        return vendor.getEmail();
    }

    public String getVendorTaxId() {
        return vendor.getTaxId();
    }
}
