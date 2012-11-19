package org.lawrencebower.docgen.web_logic.view.contact;

public class Contact {

    private final String name;
    private final String contactName;
    private final String phone;
    private final String country;
    private final String address;
    private final String taxId;
    private final String email;

    public Contact(String name,
                   String contactName,
                   String address,
                   String phone,
                   String country) {
        this(name,
             contactName,
             address,
             phone,
             country,
             null,
             null);
    }

    public Contact(String name,
                   String contactName,
                   String address,
                   String phone,
                   String country,
                   String taxId,
                   String email) {

        this.name = name;
        this.contactName = contactName;
        this.address = address;
        this.phone = phone;
        this.country = country;
        this.taxId = taxId;
        this.email = email;
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
}
