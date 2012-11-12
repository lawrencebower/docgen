package org.lawrencebower.docgen.web_model.view.contact;

public class ContactView {

    private Contact contact;

    public ContactView(Contact contact) {
        this.contact = contact;
    }

    public String getName(){
        return contact.getName();
    }

    public String getAddress(){
        return contact.getAddress();
    }

    public Contact getContact() {
        return contact;
    }
}
