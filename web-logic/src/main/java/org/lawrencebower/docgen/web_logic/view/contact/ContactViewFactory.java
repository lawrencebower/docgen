package org.lawrencebower.docgen.web_logic.view.contact;

public abstract class ContactViewFactory {

    public ContactView createContactView(Contact contact){
        ContactView contactView = createDocumentInfoView();
        contactView.setContact(contact);
        return contactView;
    }

    public abstract ContactView createDocumentInfoView();

}
