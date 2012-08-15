package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomComponentRenderer implements DocComponentRenderer<DocComponent, CustomComponentRendererInfo> {

    @Autowired
    private CustomTextRenderer textRenderer;
    @Autowired
    private CustomTableRenderer tableRenderer;

    @Override
    public void renderComponent(DocComponent component, CustomComponentRendererInfo rendererInfo) {

        if (component instanceof TextComponent) {
            textRenderer.renderComponent((TextComponent) component, rendererInfo);
        }else if (component instanceof TableComponent) {
            tableRenderer.renderComponent((TableComponent) component, rendererInfo);
        }else if (component instanceof CheckBoxComponent) {
            throw new UnsupportedOperationException("Check box not supported by Custom renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }

}
