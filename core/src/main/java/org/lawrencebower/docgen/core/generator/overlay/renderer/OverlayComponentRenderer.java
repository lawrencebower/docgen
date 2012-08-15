package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.springframework.beans.factory.annotation.Autowired;

public class OverlayComponentRenderer implements DocComponentRenderer<DocComponent, OverlayComponentRendererInfo> {

    @Autowired
    private OverlayTextRenderer textRenderer;
    @Autowired
    private OverlayTableRenderer tableRenderer;
    @Autowired
    private OverlayCheckBoxRenderer checkBoxRenderer;

    @Override
    public void renderComponent(DocComponent component, OverlayComponentRendererInfo rendererInfo) {

        if (component instanceof TextComponent) {
            textRenderer.renderComponent((TextComponent) component, rendererInfo);
        }else if (component instanceof TableComponent) {
            tableRenderer.renderComponent((TableComponent) component, rendererInfo);
        }else if (component instanceof CheckBoxComponent) {
            checkBoxRenderer.renderComponent((CheckBoxComponent) component, rendererInfo);
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }
    }
}
