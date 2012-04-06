package org.multibit.viewsystem.commandline.view;

import java.util.Collection;

import javax.swing.Action;

import org.multibit.viewsystem.View;

public interface CommandLineView extends View {
    public void setPossibleActions(Collection<Action> possibleActions);
}
