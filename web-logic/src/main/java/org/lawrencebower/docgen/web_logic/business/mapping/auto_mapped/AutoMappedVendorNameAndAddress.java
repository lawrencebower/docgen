package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorNameAndAddress extends AutoMappedComponent {

    public AutoMappedVendorNameAndAddress() {
        name = VENDOR_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String vendorName = info.getVendorName();
        String vendorAddress = info.getVendorAddress();
        String vendorCountry = info.getVendorCountry();

        String value = vendorName + "\n" + vendorAddress + "\n" + vendorCountry;

        setComponentValueIfMatch(docComponentView, value);
    }
}
