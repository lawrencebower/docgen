package org.lawrencebower.docgen.web_model.view.contact;

public abstract class ContactViewFactory {

    public ContactView createContactView(Contact contact){
        ContactView contactView = createContactView();
        contactView.setContact(contact);
        return contactView;
    }

    public abstract ContactView createContactView();

}
