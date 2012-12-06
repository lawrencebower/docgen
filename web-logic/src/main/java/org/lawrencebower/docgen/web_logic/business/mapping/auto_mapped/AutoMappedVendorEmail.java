package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorEmail extends AutoMappedComponent {

    public static final String VENDOR_EMAIL = "AUTO_MAPPED_VENDOR_EMAIL";

    public AutoMappedVendorEmail() {
        name = VENDOR_EMAIL;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorEmail();
        setComponentValueIfMatch(docComponentView, value);
    }
}
