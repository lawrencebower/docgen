package org.lawrencebower.docgen.web_model.view.contact;

import org.lawrencebower.docgen.web_model.view.view_factory.Attributes;

public class Contact {

    private String name;
    private String contactName;
    private String phone;
    private String country;
    private String address;
    private String taxId;
    private String email;
    private Attributes attributes;

    protected Contact() {//only make with a ContactBuilder
    }

    protected void setName(String name) {
        this.name = name;
    }

    protected void setContactName(String contactName) {
        this.contactName = contactName;
    }

    protected void setPhone(String phone) {
        this.phone = phone;
    }

    protected void setCountry(String country) {
        this.country = country;
    }

    protected void setAddress(String address) {
        this.address = address;
    }

    protected void setTaxId(String taxId) {
        this.taxId = taxId;
    }

    protected void setEmail(String email) {
        this.email = email;
    }

    protected void setAttributes(String... attributes) {
        this.attributes = new Attributes(attributes);
    }

    public String getName() {
        return name;
    }

    public String getAddress() {
        return address;
    }

    public String getCountry() {
        return country;
    }

    public String getPhone() {
        return phone;
    }

    public String getContactName() {
        return contactName;
    }

    public String getTaxId() {
        return taxId;
    }

    public String getEmail() {
        return email;
    }

    public Attributes getAttributes() {
        return attributes;
    }

    public boolean isAttributesMatch(Attributes attributes) {
        return this.attributes.isAttributeMatch(attributes);
    }
}
