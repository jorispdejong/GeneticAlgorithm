using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

public class TennisGame : MonoBehaviour
{
    public GameObject tennisBall;
    private List<Dictionary<string,object>> data;
    private int current = 0;

    // Start is called before the first frame update
    void Start()
    {
        // ReadCSV(@"/Users/dunnkers/git/GeneticAlgorithm/test_data.txt");
        Instantiate(
            tennisBall, new Vector3(0, 0, 0), Quaternion.identity
        );

        // read in "csv" data
        data = CSVReader.Read("test_data");
        print("CSV file read.");
    }

    // Update is called once per frame
    void Update()
    {
        if (current >= data.Count) { // reset when all positions looped.
            current = 0;
        }
        
        // transform.position += Vector3.forward * Time.deltaTime;
        float x = Convert.ToSingle(data[current]["V2"]);
        float y = Convert.ToSingle(data[current]["V3"]);
        float z = Convert.ToSingle(data[current]["V4"]);
        print("stop!");
        tennisBall.transform.position = new Vector3(x, y, z);
        current ++;
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
