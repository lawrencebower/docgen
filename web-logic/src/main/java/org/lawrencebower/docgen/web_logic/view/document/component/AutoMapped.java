package org.lawrencebower.docgen.web_logic.view.document.component;

public interface AutoMapped {

    void mapComponent(DocComponentView docComponentView,
                      AMComponentInfo info);

    boolean matchesName(String name);
}
