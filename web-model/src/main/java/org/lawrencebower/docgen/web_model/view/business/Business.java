package org.lawrencebower.docgen.web_model.view.business;

public class Business {

    private String name;
    private String contactName;
    private String phone;
    private String country;
    private String address;
    private String taxId;
    private String email;

    public Business(String name,
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

    public Business(String name,
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
