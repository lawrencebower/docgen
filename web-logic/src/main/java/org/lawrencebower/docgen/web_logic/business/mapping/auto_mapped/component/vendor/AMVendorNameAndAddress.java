package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

public class AMVendorNameAndAddress extends AbstractAMComponent {

    public AMVendorNameAndAddress() {
        name = AutoMappedField.VENDOR_NAME_AND_ADDRESS;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String vendorName = info.getVendorName();
        String vendorAddress = info.getVendorAddress();
        String vendorCountry = info.getVendorCountry();

        String value = vendorName + "\n" + vendorAddress + "\n" + vendorCountry;

        setComponentValueIfMatch(docComponentView, value);
    }
}
