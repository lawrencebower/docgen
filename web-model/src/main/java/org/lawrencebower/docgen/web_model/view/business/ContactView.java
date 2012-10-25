package org.lawrencebower.docgen.web_model.view.business;

public class ContactView {

    private Contact contact;

    public ContactView(Contact contact) {
        this.contact = contact;
    }

    public String getContactName(){
        return contact.getName();
    }

    public String getContactAddress(){
        return contact.getAddress();
    }

    public Contact getContact() {
        return contact;
    }
}
