using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.UI;

public class TennisGame : MonoBehaviour
{
    public GameObject tennisBall;
    public GameObject tennisBallInstance;
    private List<Dictionary<string,object>> data;
    private int current = 0;

    private GUIStyle guiStyle = new GUIStyle();

    // Start is called before the first frame update
    void Start()
    {
        // ReadCSV(@"/Users/dunnkers/git/GeneticAlgorithm/test_data.txt");
        tennisBallInstance = Instantiate(
            tennisBall, new Vector3(0, 0, 0), Quaternion.identity
        );

        // read in "csv" data
        data = CSVReader.Read("test_data");
        print("CSV file read.");
    }

    void OnGUI()
    {
        int px = 30;
        guiStyle.fontSize = px;

        GUI.Label(new Rect(0, px * 0, 100, px), 
            "FPS: " + (int)(1.0f / Time.smoothDeltaTime), guiStyle);

        GUI.Label(new Rect(0, px * 1, 100, px), 
            "Generation: x", guiStyle);
    }

    // Update is called once per frame
    void Update()
    {
        if (current >= data.Count) { // reset when all positions looped.
            current = 0;
        }
        
        // transform.position += Vector3.forward * Time.deltaTime;
        float z = Convert.ToSingle(data[current]["V3"]);
        float x = Convert.ToSingle(data[current]["V4"]);
        float y = Convert.ToSingle(data[current]["V5"]);
        tennisBallInstance.transform.position = new Vector3(x, y, z);

        int fps = (int) (1.0f / Time.smoothDeltaTime);
        if (fps < 0) {
            current += 1;
        } else {
            current += fps;
        }
    }

    // void ReadCSV(string filepath)
    // {
    //     using(var reader = new StreamReader(filepath))
    //     {
    //         List<string> listA = new List<string>();
    //         List<string> listB = new List<string>();
    //         bool head = false;
    //         while (!reader.EndOfStream)
    //         {
    //             var line = reader.ReadLine();
    //             var values = line.Split('\t');

    //             if (!head) {
    //                 head = true;
    //                 continue;
    //             }

    //             listA.Add(values[0]);
    //             listB.Add(values[1]);
    //         }
    //     }

    // }
}
