package Uebung2.Aufgabe1;

import java.awt.Button;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.Label;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class CounterGUI extends Frame {
	
	Counter counter;
	Label lblCount;
	
	public CounterGUI(Counter counter, int startValue) {
		this.counter = counter;
		
		final Frame frame = new Frame();
		frame.setTitle("frame");
		frame.setSize(350, 100);
		frame.setLocation(50, 50);
		
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent event) {
				counter.close(CounterGUI.this);
				frame.dispose();
			}
		});

		frame.setLayout(new FlowLayout());

		lblCount = new Label();
		lblCount.setText("" + startValue);
		frame.add(lblCount);

		Button btnStop = new Button("Stop");
		btnStop.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				counter.stop();
			}
		});
		frame.add(btnStop);

		Button btnStart = new Button("Start");
		btnStart.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				counter.start();
			}
		});
		frame.add(btnStart);

		Button btnClose = new Button("Close");
		btnClose.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				counter.close(CounterGUI.this);
				frame.dispose();
			}
		});
		frame.add(btnClose);

		Button btnCopy = new Button("Copy");
		btnCopy.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				counter.copyCounter();
			}
		});
		frame.add(btnCopy);

		Button btnClone = new Button("Clone");
		btnClone.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				counter.cloneCounter();
			}
		});
		frame.add(btnClone);
		
		frame.setVisible(true);
	}

	
	public void notify(int value) {
		lblCount.setText("" + value);
	}
	
}
