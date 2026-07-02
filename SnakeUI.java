import javax.swing.*;
import java.awt.*;
import java.io.*;

public class SnakeUI extends JFrame {
    public JButton run = new JButton("Run");
    JTextArea code = new JTextArea();
    JTextArea out = new JTextArea();
    JTextArea err = new JTextArea();
    JButton load = new JButton("Load");
    JButton save = new JButton("Save");
    JTextField filename = new JTextField(".txt");
    JScrollPane scroll = new JScrollPane(code);
    SnakeUI(){
        out.setEditable(false);
        out.setLineWrap(true);
        out.setWrapStyleWord(true);
        out.setOpaque(false);
        err.setEditable(false);
        err.setLineWrap(true);
        err.setWrapStyleWord(true);
        err.setOpaque(false);
        out.setText("std::out\n");
        err.setText("std::err\n");
        run.addActionListener(e -> {
            out.setText("std::out\n");
            err.setText("std::err\n");
            //String dir =System.getProperty("user.home");
            //File file = new File(dir+"/SnakeLang");
            //if(!file.exists()){
             //   boolean directoryCreated = file.mkdir();
             //   System.out.println(directoryCreated);

            //}

            try(BufferedWriter bw = new BufferedWriter(new FileWriter("run.txt"));){
                bw.write(code.getText());
                IO.println(code.getText());
            }
            catch(Exception ex){
                ex.printStackTrace();
            }
            File compiler = new File(System.getProperty("user.dir"));
            System.out.println(compiler.getAbsolutePath());
            System.out.println(compiler.exists());
            System.out.println(compiler.canExecute());
            ProcessBuilder pb =
                    new ProcessBuilder(compiler + "/Snakev01");
            pb.directory(compiler);
            try {
                Process p = pb.start();
                BufferedWriter writer =
                        new BufferedWriter(
                                new OutputStreamWriter(
                                        p.getOutputStream()
                                )
                        );
                BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
                String line;
                BufferedReader error = new BufferedReader(new InputStreamReader(p.getErrorStream()));
                writer.write(code.getText());
                writer.newLine();
                writer.flush();
                writer.close();
                int exitCode = p.waitFor();
                while ((line = error.readLine()) != null) {
                    err.append(line + "\n");
                }
                while ((line = br.readLine()) != null) {
                    out.append(line + "\n");
                }
                if(exitCode == 0){
                    err.setForeground(Color.black);
                    err.append("Execution was successful: Exit Code 0" + "\n");
                }
                else{
                    err.setForeground(Color.red);
                    err.append("Execution failed Exit Code: " + exitCode + "\n");
                }
                System.out.println("Exit code: " + exitCode);
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            } catch (InterruptedException ex) {
                throw new RuntimeException(ex);
            }
        });
        save.addActionListener(e -> {
            try(BufferedWriter br = new BufferedWriter(new FileWriter(filename.getText()))){
                br.write(code.getText());
            }
            catch(Exception ex){
                ex.printStackTrace();
            }
        });
        load.addActionListener(e -> {
            try(BufferedReader br = new BufferedReader(new FileReader(filename.getText()))){
                String line;
                while( (line = br.readLine()) != null){
                    code.append(line + "\n");
                }
            }
            catch(Exception ex){
                ex.printStackTrace();
            }
        });
        run.setEnabled(true);
        code.setEditable(true);
        setLayout(new GridLayout(3, 1));
        setTitle("Snake");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(1000,1000);
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout());
        panel.add(run);
        panel.add(save);
        filename.setPreferredSize(new Dimension(100,20));
        panel.add(filename);
        panel.add(load);
        JPanel outpt = new JPanel();
        outpt.setLayout(new GridLayout(1, 2));
        add(panel);
        add(scroll);
        outpt.add(out);
        outpt.add(err);
        add(outpt);
        run.setPreferredSize(new Dimension(100,30));
        code.setPreferredSize(new Dimension(1000,600));
        setVisible(true);
        setLocationRelativeTo(null);
        setResizable(false);
    }
    public static void main(String[] args) {
        SnakeUI snakeUI = new SnakeUI();
    }
}
