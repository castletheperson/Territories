import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Subscription } from 'rxjs/Subscription';

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

  territories: any = [];

  constructor(public http: HttpClient) { }

  ngOnInit() {
    this.http.get('/api/territories').subscribe((territories) => {
      this.territories = territories;
    });
  }

  newTerritory() {

  }

}
