package org.lawrencebower.docgen.web_model.view.contact;

public abstract class ContactViewFactory {

    public ContactView createContactView(Contact contact, String id){
        ContactView contactView = createContactView();
        contactView.setContact(contact);
        contactView.setContactId(id);
        return contactView;
    }

    public abstract ContactView createContactView();

}
