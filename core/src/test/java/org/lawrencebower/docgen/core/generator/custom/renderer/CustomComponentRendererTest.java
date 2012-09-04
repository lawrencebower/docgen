package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.DocComponentType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.fail;

public class CustomComponentRendererTest {

    @Test
    public void testCreateAndRenderComponent_passUnknownComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent unknownComponent = getUnknownComponent();

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.createAndRenderComponent(unknownComponent, rendererInfo);
        } catch (DocGenException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateAndRenderComponent_passCheckboxComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent checkboxComponent = new CheckBoxComponent();

        CustomComponentRendererInfo rendererInfo = new CustomComponentRendererInfo(new ByteArrayOutputStream());

        try {
            renderer.createAndRenderComponent(checkboxComponent, rendererInfo);
        } catch (UnsupportedOperationException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateComponent_passUnknownComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent unknownComponent = getUnknownComponent();

        try {
            renderer.createComponent(unknownComponent);
        } catch (DocGenException e) {
            return;
        }

        fail();//should not get here
    }

    @Test
    public void testCreateComponent_passCheckboxComponent_throwsError() throws Exception {

        CustomComponentRenderer renderer = new CustomComponentRenderer();

        DocComponent checkboxComponent = new CheckBoxComponent();

        try {
            renderer.createComponent(checkboxComponent);
        } catch (UnsupportedOperationException e) {
            return;
        }

        fail();//should not get here
    }

    private DocComponent getUnknownComponent() {
        return new DocComponent() {
            @Override
            public DocComponentType getComponentType() {
                return null;
            }
        };
    }
}
