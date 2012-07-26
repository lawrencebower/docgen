package org.lawrencebower.docgen.core.document.component.position;

import com.lowagie.text.Element;
import org.lawrencebower.docgen.core.exception.DocGenException;

public enum DocAlignment {
    LEFT,
    RIGHT,
    JUSTIFIED,
    CENTER;

    public static int mapToITextAlignment(DocAlignment alignment){
        switch (alignment){
            case LEFT: return Element.ALIGN_LEFT;
            case CENTER: return Element.ALIGN_CENTER;
            case JUSTIFIED: return Element.ALIGN_JUSTIFIED;
            case RIGHT: return Element.ALIGN_RIGHT;
        }
        throw new DocGenException("No mapping for alignment " + alignment);
    }
}
