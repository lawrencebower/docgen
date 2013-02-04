package org.lawrencebower.docgen.web_model.view.contact;

public class ContactBuilder {

    private Contact contact;

    public ContactBuilder() {
        contact = new Contact();
    }

    public void setName(String name) {
        contact.setName(name);
    }

    public void setContactName(String contactName) {
        contact.setContactName(contactName);
    }

    public void setPhone(String phone) {
        contact.setPhone(phone);
    }

    public void setCountry(String country) {
        contact.setCountry(country);
    }

    public void setAddress(String address) {
        contact.setAddress(address);
    }

    public void setTaxId(String taxId) {
        contact.setTaxId(taxId);
    }

    public void setEmail(String email) {
        contact.setEmail(email);
    }

    public void setContactId(String id){
        contact.setContactId(id);
    }

    public void setAttributes(String... attributes) {
        contact.setAttributes(attributes);
    }

    public Contact buildContact(){
        checkRequiredFields();
        return contact;
    }

    private void checkRequiredFields() {
        //todo
    }
}
